-- マウスを無効にして、コピーできるようにする
vim.opt.mouse = ""

-- 行番号非表示
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.signcolumn = "no"

-- スクロールのアニメーションを無効化
vim.g.snacks_animate = false

-- 他のエディタによるファイルの変更を反映
vim.opt.autoread = true
