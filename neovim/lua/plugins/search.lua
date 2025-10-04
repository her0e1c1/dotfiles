return {
  -- '/' を fzf-lua のバッファ内ファジー検索に差し替え
  {
    "ibhagwan/fzf-lua",
    keys = {
      {
        "/",
        function()
          local fzf = require("fzf-lua")
          fzf.blines({
            prompt = "/ ",
          })
        end,
        mode = { "n", "v" },
        desc = "Fuzzy search lines (better /)",
      },
      {
        "g/",
        "/",
        mode = { "n", "v" },
        remap = true,
        desc = "Native search (original /)",
      },
    },
  },
}
