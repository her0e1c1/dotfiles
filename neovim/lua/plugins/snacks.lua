return {
  {
    "folke/snacks.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      picker = {
        sources = {
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

