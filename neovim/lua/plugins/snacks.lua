-- Resolve the directory that should act as "here" for picker actions.
-- netrw buffers store a directory directly, normal file buffers use the file's parent directory,
-- and unnamed buffers fall back to the current working directory.
local function current_buffer_dir()
  if vim.bo.filetype == "netrw" and vim.b.netrw_curdir and vim.b.netrw_curdir ~= "" then
    return vim.b.netrw_curdir
  end

  local path = vim.api.nvim_buf_get_name(0)
  if path == "" then
    return vim.uv.cwd() or vim.fn.getcwd()
  end

  if vim.fn.isdirectory(path) == 1 then
    return path
  end

  return vim.fs.dirname(path)
end

-- Declared early because helpers above call this before its full definition appears below.
local push_netrw_directory_history

-- Return Neovim's current working directory in one place so other helpers do not repeat the fallback logic.
local function current_working_dir()
  return vim.uv.cwd() or vim.fn.getcwd()
end

-- Open a directory with netrw after recording it in our custom directory history.
-- This keeps direct opens and picker-based opens consistent.
local function open_directory(dir)
  push_netrw_directory_history(dir)
  vim.cmd.edit(dir)
end

-- Read one directory and convert its entries into Snacks picker items.
-- `want_dirs` filters the result to directories only, files only, or everything when it is nil.
local function list_directory_entries(dir, want_dirs)
  local items = {}

  for name, entry_type in vim.fs.dir(dir) do
    local is_dir = entry_type == "directory"
    if want_dirs == nil or is_dir == want_dirs then
      items[#items + 1] = {
        cwd = dir,
        dir = is_dir,
        file = name,
        text = name,
      }
    end
  end

  table.sort(items, function(a, b)
    return a.file < b.file
  end)

  return items
end

-- Build plain-text preview content for a directory history item.
-- Directories are listed first so the preview feels similar to a simple terminal listing.
local function directory_preview_text(dir)
  local directories = {}
  local files = {}

  for name, entry_type in vim.fs.dir(dir) do
    if entry_type == "directory" then
      directories[#directories + 1] = name .. "/"
    else
      files[#files + 1] = name
    end
  end

  table.sort(directories)
  table.sort(files)

  local lines = {}
  for _, name in ipairs(directories) do
    lines[#lines + 1] = name
  end
  for _, name in ipairs(files) do
    lines[#lines + 1] = name
  end

  if #lines == 0 then
    return "(empty)"
  end

  local max_lines = 200
  if #lines > max_lines then
    local hidden = #lines - max_lines
    lines = vim.list_slice(lines, 1, max_lines)
    lines[#lines + 1] = ""
    lines[#lines + 1] = ("... and %d more"):format(hidden)
  end

  return table.concat(lines, "\n")
end

-- Shared wrapper for directory-based pickers.
-- It handles the common picker setup so each caller only needs to describe its title and confirm behavior.
local function pick_directory_entries(opts)
  local target_dir = opts.dir or current_buffer_dir()

  Snacks.picker.pick({
    title = opts.title,
    items = list_directory_entries(target_dir, opts.want_dirs),
    format = opts.format or "file",
    confirm = function(picker, item)
      if not item then
        return
      end

      opts.confirm(picker, item, target_dir)
    end,
  })
end

-- Reuse Snacks' built-in "open selected file" action from custom pickers.
local function jump_to_picker_item(picker, item)
  Snacks.picker.actions.jump(picker, item, {})
end

-- Show the current directory as a simple entry list.
-- Choosing a directory opens netrw there, while choosing a file jumps to that file.
local function snacks_pick_directory_entries(dir)
  pick_directory_entries({
    dir = dir,
    title = "Dir Entries",
    format = function(item)
      return { { item.file } }
    end,
    confirm = function(picker, item, target_dir)
      if item.dir then
        picker:close()
        open_directory(vim.fs.joinpath(target_dir, item.file))
        return
      end

      jump_to_picker_item(picker, item)
    end,
  })
end

-- Show only files from a target directory and open the selected file.
local function snacks_pick_files_under_directory(dir)
  pick_directory_entries({
    dir = dir,
    title = "Directory Files",
    want_dirs = false,
    confirm = function(picker, item)
      jump_to_picker_item(picker, item)
    end,
  })
end

-- Let the user choose a subdirectory first, then narrow the next picker to files under that directory.
local function snacks_pick_subdirectory_for_files(dir)
  pick_directory_entries({
    dir = dir,
    title = "Subdirectories",
    want_dirs = true,
    confirm = function(picker, item, target_dir)
      picker:close()
      snacks_pick_files_under_directory(vim.fs.joinpath(target_dir, item.file))
    end,
  })
end

-- Combine our in-memory and netrw-provided directory history into one picker list.
-- Duplicate paths are removed so the history does not show the same directory twice.
local function list_netrw_directory_history()
  local items = {}
  local seen = {}

  -- Add one directory to the picker list together with a lightweight preview of its contents.
  local function add(dir)
    if not dir or dir == "" or seen[dir] then
      return
    end

    seen[dir] = true
    items[#items + 1] = {
      dir = dir,
      text = dir,
      preview = {
        text = directory_preview_text(dir),
        ft = "text",
        loc = false,
      },
    }
  end

  for _, dir in ipairs(vim.g.netrw_dir_history or {}) do
    add(dir)
  end

  local histmax = vim.g.netrw_dirhistmax or 0
  local histcnt = vim.g.netrw_dirhistcnt
  if histmax > 0 and histcnt ~= nil then
    local index = histcnt
    local first = true
    while first or index ~= histcnt do
      add(vim.g["netrw_dirhist_" .. index])
      first = false
      index = (index - 1) % histmax
      if index < 0 then
        index = index + histmax
      end
    end
  end

  return items
end

-- Keep a most-recent-first list of visited directories for custom netrw pickers.
-- Existing entries are moved to the front instead of being stored twice.
push_netrw_directory_history = function(dir)
  if not dir or dir == "" then
    return
  end

  local history = vim.g.netrw_dir_history or {}
  history = vim.tbl_filter(function(item)
    return item ~= dir
  end, history)
  table.insert(history, 1, dir)
  vim.g.netrw_dir_history = history
end

-- Show the saved directory history and reopen the chosen entry with netrw.
local function snacks_pick_netrw_directory_history()
  local items = list_netrw_directory_history()
  if #items == 0 then
    vim.notify("No netrw directory history", vim.log.levels.INFO)
    return
  end

  Snacks.picker.pick({
    title = "netrw history",
    items = items,
    preview = "preview",
    format = function(item)
      return { { item.dir } }
    end,
    confirm = function(picker, item)
      if not item then
        return
      end
      picker:close()
      open_directory(item.dir)
    end,
  })
end

return {
  {
    "folke/snacks.nvim",
    lazy = false,
    priority = 1000,
    keys = {
      {
        "/",
        function()
          Snacks.picker.lines()
        end,
        mode = { "n", "v" },
        desc = "Buffer Lines",
      },
      {
        "g/",
        "/",
        mode = { "n", "v" },
        remap = true,
        desc = "Native search (original /)",
      },
      {
        "<leader><space>",
        function()
          Snacks.picker.files({ cwd = current_buffer_dir() })
        end,
        desc = "Find Files (Current Dir)",
      },
      {
        "<leader>l",
        function()
          snacks_pick_directory_entries()
        end,
        desc = "Pick Buffer Dir Entries",
      },
      {
        "<leader>o",
        function()
          open_directory(current_buffer_dir())
        end,
        desc = "Open Current Directory",
      },
      {
        "<leader>O",
        function()
          snacks_pick_netrw_directory_history()
        end,
        desc = "Pick netrw Directory History",
      },
      {
        "<leader>L",
        function()
          snacks_pick_subdirectory_for_files()
        end,
        desc = "Pick Subdirectory Files",
      },
      {
        "<leader>f?",
        function()
          Snacks.picker.keymaps()
        end,
        desc = "Find Keymaps",
      },
      {
        "<leader>z",
        function()
          Snacks.zen()
        end,
        desc = "Toggle Zen Mode",
      },
    },
    opts = {
      dashboard = {
        preset = {
          header = [[
   ███████╗███╗   ██╗ █████╗  ██████╗██╗  ██╗███████╗
   ██╔════╝████╗  ██║██╔══██╗██╔════╝██║ ██╔╝██╔════╝
   ███████╗██╔██╗ ██║███████║██║     █████╔╝ ███████╗
   ╚════██║██║╚██╗██║██╔══██║██║     ██╔═██╗ ╚════██║
   ███████║██║ ╚████║██║  ██║╚██████╗██║  ██╗███████║
   ╚══════╝╚═╝  ╚═══╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚══════╝
          ]],
          ---@type snacks.dashboard.Item[]
          keys = {
            {
              icon = " ",
              key = "f",
              desc = "Find File",
              action = function()
                Snacks.dashboard.pick("files")
              end,
            },
            {
              icon = " ",
              key = "g",
              desc = "Find Text",
              action = function()
                Snacks.dashboard.pick("live_grep")
              end,
            },
            {
              icon = " ",
              key = "r",
              desc = "Recent Files",
              action = function()
                Snacks.dashboard.pick("oldfiles")
              end,
            },
            {
              icon = " ",
              key = "c",
              desc = "Config",
              action = function()
                Snacks.dashboard.pick("files", { cwd = vim.fn.stdpath("config") })
              end,
            },
            {
              icon = " ",
              key = "?",
              desc = "Keymaps",
              action = function()
                Snacks.picker.keymaps()
              end,
            },
            { icon = " ", key = "s", desc = "Restore Session", section = "session" },
            {
              icon = " ",
              key = "o",
              desc = "Open netrw (cwd)",
              action = function()
                open_directory(current_working_dir())
              end,
            },
            {
              icon = " ",
              key = "O",
              desc = "netrw History",
              action = function()
                snacks_pick_netrw_directory_history()
              end,
            },
            {
              icon = " ",
              key = "l",
              desc = "Files (cwd)",
              action = function()
                snacks_pick_directory_entries(current_working_dir())
              end,
            },
            { icon = " ", key = "q", desc = "Quit", action = ":qa" },
          },
        },
        sections = {
          { section = "header" },
          { section = "keys", gap = 1, padding = 1 },
          {
            pane = 2,
            icon = " ",
            title = "Recent Files",
            section = "recent_files",
            limit = 8,
            indent = 2,
            padding = 1,
          },
          { pane = 2, icon = " ", title = "Projects", section = "projects", indent = 2, padding = 1 },
          { section = "startup" },
        },
      },
      explorer = {
        enabled = false,
        -- Keep directory buffers on netrw so :edit <dir> follows the netrw workflow.
        replace_netrw = false,
      },
      zen = {
        toggles = {
          indent = false,
        },
        win = {
          width = 0,
        },
      },
      picker = {
        layout = {
          fullscreen = true,
        },
        sources = {
          files = {
            hidden = true,
            exclude = { "**/__pycache__/**", "*.pyc", "*.pyo" },
          },
          grep = {
            hidden = true,
            args = { "--glob", "!__pycache__/**", "--glob", "!*.pyc", "--glob", "!*.pyo" },
          },
        },
      },
    },
    config = function(_, opts)
      require("snacks").setup(opts)
    end,
  },
}
