local function current_buffer_dir()
  local path = vim.api.nvim_buf_get_name(0)
  if path == "" then
    return vim.uv.cwd() or vim.fn.getcwd()
  end

  return vim.fs.dirname(path)
end

local function snacks_pick_directory_entries(dir)
  -- Ubuntu packages fd as `fdfind`, while macOS/Homebrew uses `fd`.
  local fd_cmd = vim.fn.executable("fd") == 1 and "fd" or "fdfind"
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Dir Entries",
    finder = function(_, ctx)
      return require("snacks.picker.source.proc").proc({
        cmd = fd_cmd,
        cwd = target_dir,
        args = {
          "--max-depth",
          "1",
          "--hidden",
          "--strip-cwd-prefix",
          "--type",
          "f",
          ".",
        },
        transform = function(item)
          item.cwd = target_dir
          item.file = item.text
          item.dir = vim.fn.isdirectory(vim.fs.joinpath(target_dir, item.file)) == 1
        end,
      }, ctx)
    end,
    format = "file",
    confirm = function(picker, item)
      if not item then
        return
      end
      if item.dir then
        picker:close()
        snacks_pick_directory_entries(vim.fs.joinpath(target_dir, item.file))
        return
      end
      Snacks.picker.actions.jump(picker, item, {})
    end,
  })
end

local function snacks_pick_subdirectory_for_explorer(dir)
  -- Ubuntu packages fd as `fdfind`, while macOS/Homebrew uses `fd`.
  local fd_cmd = vim.fn.executable("fd") == 1 and "fd" or "fdfind"
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Subdirectories",
    finder = function(_, ctx)
      return require("snacks.picker.source.proc").proc({
        cmd = fd_cmd,
        cwd = target_dir,
        args = {
          "--hidden",
          "--strip-cwd-prefix",
          "--type",
          "d",
          ".",
        },
        transform = function(item)
          item.cwd = target_dir
          item.file = item.text
        end,
      }, ctx)
    end,
    format = "file",
    confirm = function(picker, item)
      if not item then
        return
      end

      picker:close()
      Snacks.explorer({ cwd = vim.fs.joinpath(target_dir, item.file) })
    end,
  })
end

local function snacks_pick_files_under_directory(dir)
  -- Ubuntu packages fd as `fdfind`, while macOS/Homebrew uses `fd`.
  local fd_cmd = vim.fn.executable("fd") == 1 and "fd" or "fdfind"
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Directory Files",
    finder = function(_, ctx)
      return require("snacks.picker.source.proc").proc({
        cmd = fd_cmd,
        cwd = target_dir,
        args = {
          "--hidden",
          "--strip-cwd-prefix",
          "--type",
          "f",
          ".",
        },
        transform = function(item)
          item.cwd = target_dir
          item.file = item.text
        end,
      }, ctx)
    end,
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
  -- Ubuntu packages fd as `fdfind`, while macOS/Homebrew uses `fd`.
  local fd_cmd = vim.fn.executable("fd") == 1 and "fd" or "fdfind"
  local target_dir = dir or current_buffer_dir()

  Snacks.picker.pick({
    title = "Subdirectories",
    finder = function(_, ctx)
      return require("snacks.picker.source.proc").proc({
        cmd = fd_cmd,
        cwd = target_dir,
        args = {
          "--hidden",
          "--strip-cwd-prefix",
          "--type",
          "d",
          ".",
        },
        transform = function(item)
          item.cwd = target_dir
          item.file = item.text
        end,
      }, ctx)
    end,
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
