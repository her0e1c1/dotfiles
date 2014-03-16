;初期位置
;(cd "~/")

;リージョンを kill-ring に入れないで削除できるようにする
(delete-selection-mode t)

;C-x C-fで大文字小文字をを区別しない
(setq completion-ignore-case t)

;eval-expression M-:でtab補完する
;(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;行と列番号を表示する
(column-number-mode t)
(line-number-mode t)

;when stating, dont split a display into two
(setq inhibit-startup-message t)

;バックアップファイルを作成しない
(setq backup-inhibited t)

;dont make #file
(setq auto-save-default nil)
(setq make-backup-files nil)

;delete で選択範囲を削除する、この時kill ringに値を格納しない
(delete-selection-mode t)

;他のアプリケーションでの貼り付け可能にする
(setq x-select-enable-clipboard t)

;括弧にカソールを合わせたときに対応関係を表示
(show-paren-mode)

;スペルチェック
(setq-default flyspell-mode t)

;実行できるスクリプトは保存時に実行権限をつける
(add-hook 'after-save-hook
        'executable-make-buffer-file-executable-if-script-p)

;メニューバーを隠す
(menu-bar-mode -1)

;選択範囲を色つけする
;(transient-mark-mode t)

;ファイル名の補完で大文字小文字を区別しない??
;(setq completion-ignore-case t)

;C-x C-fで大文字小文字をを区別しない??
;(setq completion-ignore-case t)


;補完可能なものを随時表示
(icomplete-mode 0)

;C-kで行全体を削除(効果なし？)
(setq kill-whole-line t)

;"yes or no"を"y or n"に
(fset 'yes-or-no-p 'y-or-n-p)

;buffer再度読み込みをします。
(global-auto-revert-mode 1)


;bufferの切り替えを楽にする
(iswitchb-mode 1)

;tをnilにすると部分一致
(setq iswitchb-regexp t)

;beep音を消す
(setq ring-bell-function 'ignore)

;スクロールを一行ずつにする
(setq scroll-step 1)
(setq scroll-conservatively 1)

;現在行を目立たせる
(global-hl-line-mode)

;時計の表示
;(display-time-mode 1)

;最終行に必ず一行挿入する
;(setq require-final-newline t)

;スクロールで画面を少し残す
(setq next-screen-context-lines 1)

;巡回する
(setq windmove-wrap-around t)

;
(setq skeleton-pair 1)


; タブの設定
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; フォントの初期値
(set-default-font "Monospace-12")

;バッファが同じ名前のときの表示名を指定する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;window 操作
;(split-window-horizontally)
;(put 'upcase-region 'disabled nil)

;termの文字化け
(setq locale-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(setenv "LANG" "ja_JP.UTF-8")

;日本語化
(prefer-coding-system 'utf-8)

;window color
; (set-background-color "Black")
; (set-foreground-color "White")
; (set-cursor-color "Gray")
; (add-to-list 'default-frame-alist '(alpha . (0.95 0.90)))
; 
; (when (fboundp 'winner-mode)
;   (winner-mode 1))

(put 'erase-buffer 'disabled nil)
;(setq explicit-shell-file-name "/bin/bash")
(setq tramp-verbose 10)
;(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(provide 'init-env)
