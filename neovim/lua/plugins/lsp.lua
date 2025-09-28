return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        pyright = {},
        gopls = {},
        tsserver = {}, -- または vtsls
      },
    },
  },
}
