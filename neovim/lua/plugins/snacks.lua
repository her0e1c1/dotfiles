-- Return Neovim's current working directory in one place so other helpers do not repeat the fallback logic.
local function current_working_dir()
  return vim.uv.cwd() or vim.fn.getcwd()
end

-- Resolve the directory that should act as "here" for picker actions.
-- netrw buffers store a directory directly, normal file buffers use the file's parent directory,
-- and unnamed buffers fall back to the current working directory.
local function current_buffer_dir()
  -- falsy values are false and nil only
  if vim.bo.filetype == "netrw" and (vim.b.netrw_curdir or "") ~= "" then
    return vim.b.netrw_curdir
  end

  local path = vim.api.nvim_buf_get_name(0) -- 0 == current buffer
  if path == "" then
    return current_working_dir()
  end
  if vim.fn.isdirectory(path) == 1 then
    return path
  end
  return vim.fs.dirname(path)
end

-- Read one directory and convert its entries into Snacks picker items.
-- `want_dirs` filters the result to directories only, files only, or everything when it is nil.
local function list_directory_entries(dir, want_dirs)
  local items = {}

  for name, type in vim.fs.dir(dir) do -- only items in cur dir
    local is_dir = type == "directory"
    if want_dirs == nil or is_dir == want_dirs then
      items[#items + 1] = { -- append last
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

-- Build the command used to render a colorized directory preview.
local function directory_preview_ls_command(dir)
  return { "ls", "-lA", "--color=always", dir }
end

-- Keep only the first occurrence of each non-empty value.
local function unique(list)
  local items = {}
  local seen = {}

  for _, value in ipairs(list) do
    if value and value ~= "" and not seen[value] then
      seen[value] = true
      items[#items + 1] = value
    end
  end

  return items
end

-- Shared wrapper for directory-based pickers.
-- It handles the common picker setup so each caller only needs to describe its title and confirm behavior.
-- Show the current directory as a simple entry list.
-- Selecting an item always jumps using Snacks' default action.
local function snacks_pick_directory_entries(dir)
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Dir Entries",
    items = list_directory_entries(target_dir, nil),
    format = "file",
    formatters = {
      file = {
        filename_only = true,
      },
    },
    confirm = function(picker, item)
      Snacks.picker.actions.jump(picker, item, {})
    end,
  })
end

-- Pick subdirectories recursively via `fdfind` and open the selected one in netrw.
local function snacks_pick_recursive_subdirectories(opts)
  opts = opts or {}
  local target_dir = opts.dir or current_buffer_dir()
  local max_depth = opts.max_depth
  local args = { "--hidden", "--strip-cwd-prefix", "--type", "d" }

  if max_depth ~= nil then
    vim.list_extend(args, { "--max-depth", tostring(max_depth) })
  end

  vim.list_extend(args, { "--exclude", ".git", "." })

  Snacks.picker.pick({
    title = "Subdirectories (recursive)",
    format = "file",
    finder = function(_, ctx)
      return require("snacks.picker.source.proc").proc({
        cmd = "fdfind",
        cwd = target_dir,
        args = args,
        transform = function(item)
          item.cwd = target_dir
          item.dir = true
          item.file = item.text
        end,
      }, ctx)
    end,
    confirm = function(picker, item)
      if not item then
        return
      end
      picker:close()
      vim.cmd.edit(vim.fs.joinpath(target_dir, item.file))
    end,
  })
end

-- Read netrw's directory history ring into a picker-friendly list.
local function list_netrw_directory_history()
  local dirs = {}
  local histmax = vim.g.netrw_dirhistmax or 0
  local histcnt = vim.g.netrw_dirhistcnt
  if histmax <= 0 or histcnt == nil then
    return {}
  end

  for offset = 0, histmax - 1 do
    local index = (histcnt - offset) % histmax
    dirs[#dirs + 1] = vim.g["netrw_dirhist_" .. index]
  end

  return unique(dirs)
end

-- Read .netrwhist directly and return only the saved directory strings.
local function list_netrw_dirhist_dirs()
  local dirs = {}
  local path = vim.fs.joinpath(vim.fn.stdpath("data"), ".netrwhist")
  local file = io.open(path, "r")

  if not file then
    return dirs
  end

  for line in file:lines() do
    dirs[#dirs + 1] = line:match("^let g:netrw_dirhist_%d+%s*=%s*'(.*)'$")
  end

  file:close()
  return unique(dirs)
end

-- Show the saved directory history and reopen the chosen entry with netrw.
local function snacks_pick_netrw_directory_history(opts)
  opts = opts or {}

  local dirs
  if opts.read_from_netrwhist then
    dirs = list_netrw_dirhist_dirs()
  else
    dirs = list_netrw_directory_history()
  end

  local items = vim.tbl_map(function(dir)
    return {
      dir = dir,
      file = dir,
      text = dir,
    }
  end, dirs)

  if #items == 0 then
    vim.notify("No netrw directory history", vim.log.levels.INFO)
    return
  end

  Snacks.picker.pick({
    title = "netrw history",
    items = items,
    preview = function(ctx)
      require("snacks.picker.preview").cmd(directory_preview_ls_command(ctx.item.dir), ctx, { pty = true })
    end,
    format = "file",
    confirm = function(picker, item)
      picker:close()
      vim.cmd.edit(item.dir)
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
          Snacks.picker.lines({
            pattern = vim.fn.expand("<cword>"),
            layout = "default",
            on_show = function(picker)
              local cursor = vim.api.nvim_win_get_cursor(picker.main)
              local info = vim.api.nvim_win_call(picker.main, vim.fn.winsaveview)
              picker.list:view(cursor[1], info.topline)
              picker:show_preview()
              picker.layout:maximize()
            end,
          })
        end,
        mode = { "n", "v" },
        desc = "Buffer Lines",
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
        "<leader>d",
        function()
          snacks_pick_recursive_subdirectories({ max_depth = 1 })
        end,
        desc = "Pick Recursive Subdirectories",
      },
      {
        "<leader>D",
        function()
          snacks_pick_recursive_subdirectories({ dir = LazyVim.root() })
        end,
        desc = "Pick Root Recursive Subdirectories",
      },
      {
        "<leader>L",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Pick Buffers",
      },
      {
        "<leader>o",
        function()
          vim.cmd.edit(current_buffer_dir())
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
              icon = " ",
              key = "d",
              desc = "Subdirectories (cwd)",
              action = function()
                snacks_pick_recursive_subdirectories({ dir = current_working_dir(), max_depth = 3 })
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
                vim.cmd.edit(current_working_dir())
              end,
            },
            {
              icon = " ",
              key = "O",
              desc = "netrw History",
              action = function()
                snacks_pick_netrw_directory_history({ read_from_netrwhist = true })
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
