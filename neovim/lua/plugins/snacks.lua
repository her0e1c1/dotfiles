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

local function list_directory_items(dir, want_dirs)
  local items = {}
  local iter = vim.fs.dir(dir)

  for name, entry_type in iter do
    local is_dir = entry_type == "directory"
    if is_dir == want_dirs then
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

local function list_subdirectory_items_recursive(dir)
  local items = {}

  local function walk(root, prefix)
    for name, entry_type in vim.fs.dir(root) do
      if entry_type == "directory" then
        local relative_path = prefix == "" and name or vim.fs.joinpath(prefix, name)
        items[#items + 1] = {
          cwd = dir,
          dir = true,
          file = relative_path,
          text = relative_path,
        }
        walk(vim.fs.joinpath(root, name), relative_path)
      end
    end
  end

  walk(dir, "")

  table.sort(items, function(a, b)
    return a.file < b.file
  end)

  return items
end

local function snacks_pick_directory_entries(dir)
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Dir Entries",
    items = list_directory_items(target_dir, false),
    format = "file",
    confirm = function(picker, item)
      if not item then
        return
      end
      Snacks.picker.actions.jump(picker, item, {})
    end,
  })
end

local function snacks_pick_subdirectory_for_explorer(dir)
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Subdirectories (recursive)",
    items = list_subdirectory_items_recursive(target_dir),
    format = "file",
    confirm = function(picker, item)
      if not item then
        return
      end

      picker:close()
      vim.cmd.edit(vim.fs.joinpath(target_dir, item.file))
    end,
  })
end

local function snacks_pick_files_under_directory(dir)
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Directory Files",
    items = list_directory_items(target_dir, false),
    format = "file",
    confirm = function(picker, item)
      if not item then
        return
      end

      Snacks.picker.actions.jump(picker, item, {})
    end,
  })
end

local function snacks_pick_subdirectory_for_files(dir)
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Subdirectories",
    items = list_directory_items(target_dir, true),
    format = "file",
    confirm = function(picker, item)
      if not item then
        return
      end

      picker:close()
      snacks_pick_files_under_directory(vim.fs.joinpath(target_dir, item.file))
    end,
  })
end

local function list_netrw_directory_history()
  local items = {}
  local seen = {}

  local function add(dir)
    if not dir or dir == "" or seen[dir] then
      return
    end

    seen[dir] = true
    items[#items + 1] = {
      dir = dir,
      label = dir,
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

local function push_netrw_directory_history(dir)
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

local function snacks_pick_netrw_directory_history()
  local items = list_netrw_directory_history()
  if #items == 0 then
    vim.notify("No netrw directory history", vim.log.levels.INFO)
    return
  end

  Snacks.picker.select(items, {
    prompt = "netrw history",
    format_item = function(item)
      return item.label
    end,
  }, function(item)
    if not item then
      return
    end
    push_netrw_directory_history(item.dir)
    vim.cmd.edit(item.dir)
  end)
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
        "<leader>e",
        function()
          local cwd = vim.fn.expand("%:p:h")
          if cwd == "" then
            cwd = vim.uv.cwd() or vim.fn.getcwd()
          end
          Snacks.explorer({ cwd = cwd })
        end,
        desc = "Open Explorer",
      },
      {
        "<leader>E",
        function()
          Snacks.explorer({ cwd = LazyVim.root() })
        end,
        desc = "Toggle Explorer",
      },
      {
        "<leader>d",
        function()
          snacks_pick_subdirectory_for_explorer()
        end,
        desc = "Pick Subdirectory Explorer",
      },
      {
        "<leader>D",
        function()
          snacks_pick_subdirectory_for_explorer(LazyVim.root())
        end,
        desc = "Pick Root Subdirectory Explorer",
      },
      {
        "<leader><space>",
        function()
          Snacks.picker.files({ cwd = current_buffer_dir() })
        end,
        desc = "Find Files (Current Dir)",
      },
      {
        "<leader>fd",
        function()
          snacks_pick_directory_entries()
        end,
        desc = "Pick Dir Entries",
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
          local dir = current_buffer_dir()
          push_netrw_directory_history(dir)
          vim.cmd.edit(dir)
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
              icon = " ",
              key = "e",
              desc = "Explorer",
              action = function()
                Snacks.explorer({ cwd = vim.uv.cwd() or vim.fn.getcwd() })
              end,
            },
            {
              icon = " ",
              key = "d",
              desc = "Directories (cwd)",
              action = function()
                snacks_pick_subdirectory_for_explorer(vim.uv.cwd() or vim.fn.getcwd())
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
              key = "l",
              desc = "Files (cwd)",
              action = function()
                snacks_pick_directory_entries(vim.uv.cwd() or vim.fn.getcwd())
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
        enabled = true,
        -- Let oil.nvim own directory buffers opened with :edit <dir>.
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
          explorer = {
            hidden = true,
            layout = {
              preset = "sidebar",
              preview = false,
              fullscreen = false,
            },
            jump = {
              close = false,
            },
            win = {
              list = {
                keys = {
                  ["p"] = "explorer_up",
                },
              },
            },
          },
          -- ファイル検索の除外
          files = {
            hidden = true, -- 必要に応じて変更
            exclude = { "**/__pycache__/**", "*.pyc", "*.pyo" },
          },
          -- ripgrep 検索の除外
          grep = {
            hidden = true,
            -- ripgrep に直接グロブを渡す
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
