-- https://www.lazyvim.org/configuration
-- ここによると、 lua/config/lazy.luaから呼び出しがある

local api = vim.api
local opt = vim.opt

-- === 基本設定 ===
-- マウスを無効にして、コピーできるようにする
opt.mouse = ""
-- swapファイルを作らない
opt.swapfile = false
-- 行番号非表示
opt.number = false
opt.relativenumber = false
opt.signcolumn = "no"
-- スクロールのアニメーションを無効化
vim.g.snacks_animate = false
-- mdの```を省略せずに表示
opt.conceallevel = 0
-- spell checkerが英語にしか対応していないため、日本語の文字列が常にエラーになるのを防ぐ
opt.spelllang = { "en", "cjk" }

-- === netrw ===
-- 組み込みファイラ netrw の見た目と操作感をここでまとめて調整する。
-- 表示設定と buffer-local keymap を近くに置いて、netrw の挙動をこの塊だけで理解できるようにする。
-- netrw のヘッダーを隠して一覧を見やすくする
vim.g.netrw_banner = 0
-- netrw を通常一覧表示にして tree 展開状態を引きずらない
vim.g.netrw_liststyle = 1
-- netrw のディレクトリ表示を毎回リフレッシュして前の状態を引きずらない
vim.g.netrw_fastbrowse = 0
-- netrw の標準ディレクトリ履歴を picker で使うため、履歴数を明示しておく
vim.g.netrw_dirhistmax = 50

local function set_netrw_keymaps(buf)
  vim.keymap.set("n", "p", "-", {
    buffer = buf,
    remap = true,
    silent = true,
    desc = "netrw Parent Directory",
  })
  vim.keymap.set("n", "o", "<leader>O", {
    buffer = buf,
    remap = true,
    silent = true,
    desc = "netrw Directory History",
  })
  vim.keymap.set("n", "a", "%", {
    buffer = buf,
    remap = true,
    silent = true,
    desc = "netrw Create File",
  })
  vim.keymap.set("n", "l", "<leader>l", {
    buffer = buf,
    remap = true,
    silent = true,
    desc = "netrw Directory Entries Picker",
  })
  vim.keymap.set("n", "?", function()
    require("which-key").show({ global = false })
  end, {
    buffer = buf,
    silent = true,
    desc = "netrw Keymaps",
  })
  vim.keymap.set("n", "g?", function()
    vim.cmd.help("netrw-browse-maps")
  end, {
    buffer = buf,
    silent = true,
    desc = "netrw Help",
  })
end

-- netrw バッファを開いたときに上の keymap を適用する
local netrw_group = api.nvim_create_augroup("custom_netrw", { clear = true })
api.nvim_create_autocmd("FileType", {
  group = netrw_group,
  pattern = "netrw",
  callback = function(event)
    set_netrw_keymaps(event.buf)
  end,
})

-- === 自動処理 ===
-- 他のエディタによるファイルの変更を反映
opt.autoread = true
-- 上で定義した静的設定を補うために、フォーカス復帰時や netrw バッファ初期化時の処理を登録する。
local autoread_group = api.nvim_create_augroup("custom_autoread", { clear = true })
api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
  group = autoread_group,
  -- バッファを開き直さなくても外部変更を取り込めるよう、適切なタイミングでディスク上の更新を確認する。
  command = "if mode() != 'c' | checktime | endif",
})
