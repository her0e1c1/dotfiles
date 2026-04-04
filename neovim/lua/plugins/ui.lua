return {
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "tokyonight",
    },
  },
  {
    "folke/noice.nvim",
    opts = {
      -- Keep transient messages out of the top-right mini/notify views.
      messages = {
        enabled = false,
      },
      notify = {
        enabled = false,
      },
      lsp = {
        progress = {
          enabled = false,
        },
        message = {
          enabled = false,
        },
      },
    },
  },
  {
    "snacks.nvim",
    opts = {
      -- Disable top-right notifications so movement stays in the buffer.
      notifier = {
        enabled = false,
      },
    },
  },
  {
    "folke/tokyonight.nvim",
    opts = {
      style = "night",
      transparent = false,
      on_colors = function(colors)
        -- Keep all major surfaces pure black to match the rest of this UI setup.
        colors.bg = "#000000"
        colors.bg_dark = "#000000"
        colors.bg_float = "#000000"
        colors.bg_sidebar = "#000000"
      end,
      on_highlights = function(highlights)
        -- Brighten comments because the default contrast is too low on this black background.
        highlights.Comment = { fg = "#A3BE8C" }
      end,
    },
  },
  {
    "nvim-tree/nvim-web-devicons",
    config = function()
      require("nvim-web-devicons").setup {
        -- オプション例
        color_icons = true,
        default = true,
      }
    end,
  },
}
