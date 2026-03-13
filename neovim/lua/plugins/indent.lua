return {
  {
    "folke/snacks.nvim",
    opts = {
      indent = {
        indent = {
          -- Render guides as tinted spaces so terminal copy does not include "|" characters.
          char = " ",
        },
        scope = {
          -- Keep the active scope consistent with the same non-text rendering approach.
          char = " ",
        },
      },
    },
    init = function()
      local function set_indent_highlights()
        -- Use subtle background shades to preserve guide visibility on a black theme.
        vim.api.nvim_set_hl(0, "SnacksIndent", { bg = "#141414" })
        vim.api.nvim_set_hl(0, "SnacksIndentScope", { bg = "#2a2a2a" })
      end

      local group = vim.api.nvim_create_augroup("custom_indent_guides", { clear = true })
      vim.api.nvim_create_autocmd("ColorScheme", {
        group = group,
        -- Re-apply custom highlights after every colorscheme change.
        callback = set_indent_highlights,
      })

      set_indent_highlights()
    end,
  },
}
