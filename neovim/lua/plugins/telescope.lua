return {
  {
    "nvim-telescope/telescope.nvim",
    opts = {
      defaults = {
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
      { "<leader>fc", "<cmd>Telescope commands<cr>", desc = "Find Commands" },
      { "<leader>fk", "<cmd>Telescope keymaps<cr>", desc = "Find Keymaps" },
      { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Find Help" },
    },
  },
}
