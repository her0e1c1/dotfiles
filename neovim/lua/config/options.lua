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

-- netrw のヘッダーを隠して一覧を見やすくする
vim.g.netrw_banner = 0
-- netrw を通常一覧表示にして tree 展開状態を引きずらない
vim.g.netrw_liststyle = 1
-- netrw のディレクトリ表示を毎回リフレッシュして前の状態を引きずらない
vim.g.netrw_fastbrowse = 0
-- netrw の標準ディレクトリ履歴を picker で使うため、履歴数を明示しておく
vim.g.netrw_dirhistmax = 50

-- 他のエディタによるファイルの変更を反映
vim.opt.autoread = true
local autoread_group = vim.api.nvim_create_augroup("custom_autoread", { clear = true })
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
  group = autoread_group,
  command = "if mode() != 'c' | checktime | endif",
})

local netrw_history_file = vim.fs.joinpath(vim.fn.stdpath("state"), "netrw-dir-history.json")

local function save_netrw_dir_history()
  local ok, encoded = pcall(vim.fn.json_encode, vim.g.netrw_dir_history or {})
  if not ok then
    return
  end

  vim.fn.mkdir(vim.fs.dirname(netrw_history_file), "p")
  vim.fn.writefile({ encoded }, netrw_history_file)
end

local function load_netrw_dir_history()
  if vim.fn.filereadable(netrw_history_file) ~= 1 then
    return
  end

  local lines = vim.fn.readfile(netrw_history_file)
  if #lines == 0 then
    return
  end

  local ok, decoded = pcall(vim.fn.json_decode, table.concat(lines, "\n"))
  if ok and type(decoded) == "table" then
    vim.g.netrw_dir_history = decoded
  end
end

load_netrw_dir_history()

local function push_netrw_dir_history(dir)
  if not dir or dir == "" then
    return
  end

  vim.g.netrw_dir_history = vim.g.netrw_dir_history or {}
  vim.g.netrw_dir_history = vim.tbl_filter(function(item)
    return item ~= dir
  end, vim.g.netrw_dir_history)
  table.insert(vim.g.netrw_dir_history, 1, dir)
  save_netrw_dir_history()
end

local function current_netrw_dir(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return nil
  end

  local dir = vim.b[buf].netrw_curdir
  if dir and dir ~= "" then
    return dir
  end

  local path = vim.api.nvim_buf_get_name(buf)
  if path ~= "" and vim.fn.isdirectory(path) == 1 then
    return path
  end

  return nil
end

local function sync_netrw_dir_history(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return
  end

  local dir = current_netrw_dir(buf)
  if not dir or dir == "" or vim.b[buf].netrw_last_history_dir == dir then
    return
  end

  vim.b[buf].netrw_last_history_dir = dir
  push_netrw_dir_history(dir)
end

local netrw_group = vim.api.nvim_create_augroup("custom_netrw", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = netrw_group,
  pattern = "netrw",
  callback = function(event)
    vim.keymap.set("n", "p", "-", {
      buffer = event.buf,
      remap = true,
      silent = true,
      desc = "netrw Parent Directory",
    })
    vim.keymap.set("n", "?", function()
      vim.cmd.help("pi_netrw")
    end, {
      buffer = event.buf,
      silent = true,
      desc = "netrw Help",
    })
    sync_netrw_dir_history(event.buf)
    vim.schedule(function()
      sync_netrw_dir_history(event.buf)
    end)
  end,
})
vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter", "CursorMoved" }, {
  group = netrw_group,
  callback = function(event)
    if vim.bo[event.buf].filetype ~= "netrw" and current_netrw_dir(event.buf) == nil then
      return
    end
    sync_netrw_dir_history(event.buf)
    vim.schedule(function()
      sync_netrw_dir_history(event.buf)
    end)
  end,
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
