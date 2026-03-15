-- 入力途中のキー候補をポップアップ表示して、キーバインドを見つけやすくする。
return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  opts = {
    -- classicに比べて、表示範囲が広くなり読みやすくなる
    preset = "modern",
  },
}
