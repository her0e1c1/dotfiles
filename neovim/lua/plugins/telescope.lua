local function current_buffer_dir()
  local path = vim.api.nvim_buf_get_name(0)
  if path == "" then
    return vim.uv.cwd() or vim.fn.getcwd()
  end

  return vim.fs.dirname(path)
end

local function telescope_pick_directory_entries(dir)
  local builtin = require("telescope.builtin")
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")
  -- Ubuntu packages fd as `fdfind`, while macOS/Homebrew uses `fd`.
  local fd_cmd = vim.fn.executable("fd") == 1 and "fd" or "fdfind"
  local target_dir = dir or current_buffer_dir()

  builtin.find_files({
    cwd = target_dir,
    prompt_title = "Dir Entries",
    find_command = {
      fd_cmd,
      "--max-depth",
      "1",
      "--hidden",
      "--strip-cwd-prefix",
      "--type",
      "f",
      "--type",
      "d",
      "--exclude",
      "__pycache__",
      "--exclude",
      "*.pyc",
      "--exclude",
      "*.pyo",
      ".",
    },
    attach_mappings = function(prompt_bufnr)
      actions.select_default:replace(function()
        local selection = action_state.get_selected_entry()
        actions.close(prompt_bufnr)
        if not selection then
          return
        end

        local relative_path = selection.path or selection.filename or selection.value or selection[1]
        if not relative_path then
          return
        end

        local path = relative_path:match("^/") and relative_path or vim.fs.joinpath(target_dir, relative_path)
        if vim.fn.isdirectory(path) == 1 then
          telescope_pick_directory_entries(path)
          return
        end

        vim.cmd.edit(vim.fn.fnameescape(path))
      end)

      return true
    end,
  })
end

return {
  {
    "nvim-telescope/telescope.nvim",
    opts = {
      defaults = {
        -- Fill the available editor area for all Telescope pickers.
        layout_strategy = "horizontal",
        layout_config = {
          width = { padding = 0 },
          height = { padding = 0 },
          preview_cutoff = 0,
          horizontal = {
            preview_width = 0.55,
          },
        },
        -- 除外したいパターンを追加
        -- Lua パターンなので . は %. でエスケープ
        -- .pyc/.pyo と __pycache__ ディレクトリを除外
        file_ignore_patterns = {
           "%.pyc$",
           "%.pyo$",
           "__pycache__/"
        },
      },
      -- find_files の列挙段階で ripgrep に除外を伝える
      pickers = {
        find_files = {
          -- fd が無くても動くよう rg を使用
          -- 必要に応じて --hidden を追加可
          find_command = {
            "rg",
            "--files",
            "-g", "!__pycache__/",
            "-g", "!*.pyc",
            "-g", "!*.pyo",
          },
        },
      },
    },
    keys = {
      {
        "<leader>d",
        function()
          telescope_pick_directory_entries()
        end,
        desc = "Pick Dir Entries",
        nowait = true,
      },
      { "<leader>fc", "<cmd>Telescope commands<cr>", desc = "Find Commands" },
      { "<leader>fk", "<cmd>Telescope keymaps<cr>", desc = "Find Keymaps" },
      { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Find Help" },
    },
  },
}
