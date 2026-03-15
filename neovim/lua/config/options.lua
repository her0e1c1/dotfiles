-- https://www.lazyvim.org/configuration
-- ここによると、 lua/config/lazy.luaから呼び出しがある

-- マウスを無効にして、コピーできるようにする
vim.opt.mouse = ""

-- swapファイルを作らない
vim.opt.swapfile = false

-- 行番号非表示
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.signcolumn = "no"

-- スクロールのアニメーションを無効化
vim.g.snacks_animate = false

-- 他のエディタによるファイルの変更を反映
vim.opt.autoread = true
local autoread_group = vim.api.nvim_create_augroup("custom_autoread", { clear = true })
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
  group = autoread_group,
  command = "if mode() != 'c' | checktime | endif",
})

--  mdの```を省略せずに表示
vim.opt.conceallevel = 0

-- specll checkerが英語にしか対応していないため、日本語の文字列が常にエラーになるのを防ぐ (underlineが表示されなくなる)
vim.opt.spelllang = { "en", "cjk" }

-- :ReloadConfigで、設定を反映させる
-- lazy vimの場合、supportされていないみたい...
-- ただし、options.luaの読み込みはされた
pcall(vim.api.nvim_del_user_command, "ReloadConfig")

vim.api.nvim_create_user_command("ReloadConfig", function()
  for name in pairs(package.loaded) do
    if name:match("^config") or name:match("^plugins") then
      package.loaded[name] = nil
    end
  end

  dofile(vim.env.MYVIMRC)
  print("Config reloaded")
end, {})
