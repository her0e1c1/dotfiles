;--------------------------------------------------
;文字コードの設定
;--------------------------------------------------

;termの文字化け
(setq locale-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(setenv "LANG" "ja_JP.UTF-8")

;; 日本語化
(prefer-coding-system 'utf-8)


;--------------------------------------------------
;window settings
;--------------------------------------------------

;window color
(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "Gray")
(add-to-list 'default-frame-alist '(alpha . (0.95 0.90)))


(when (fboundp 'winner-mode)
  (winner-mode 1))

;起動時にwindowを最大化する
(when window-system 
    (let (
          (px (display-pixel-width)) 
          (py (display-pixel-height)) 
          (fx (frame-char-width)) 
          (fy (frame-char-height)) 
          tx ty 
          ) 
      ;delete tool bar (don't use this when running on CUI)
      (tool-bar-mode 0)
      ;スクロールバーを消す (don't use this on CUI)
      (toggle-scroll-bar nil)
      ;; Next formulas discovered empiric on Windows/Linux host 
      ;; with default font (7x13). 
      (setq tx (- (/ px fx) 7)) 
      (setq ty (- (/ py fy) 4)) 
      (setq initial-frame-alist '((top . 2) (left . 2))) 
      (add-to-list 'initial-frame-alist (cons 'width tx)) 
      (add-to-list 'initial-frame-alist (cons 'height ty))))

;--------------------------------------------------
;tab settings
;--------------------------------------------------

;M-4 C-x tab もまとめたい
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)



;--------------------------------------------------
;OSごとの環境設定
;--------------------------------------------------

(if (or (eq window-system 'mac)
        (eq window-system 'ns))
    (set-frame-font "Hiragino-12")
    (let ()
	  ;フォントサイズを変更する
      ;(require 'carbon-font)
      ;(fixed-width-set-fontset "hirakaku_w3" 12)
      )
  (set-default-font "Monospace-12")
  )


;--------------------------------------------------
;Settings for emacs
;--------------------------------------------------

;C-x C-fで大文字小文字をを区別しない
(setq completion-ignore-case t)

;eval-expression M-:でtab補完する
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;行と列番号を表示する
(column-number-mode t)
(line-number-mode t)

;when stating, dont split a display into two
(setq inhibit-startup-message t)

;dont make backup file
(setq backup-inhibited t)

;dont make #file
(setq auto-save-default nil)
(setq make-backup-files nil)

;delete で選択範囲を削除する、この時kill ringに値を格納しない
(delete-selection-mode t)

;他のアプリケーションでの貼り付け可能にする
(setq x-select-enable-clipboard t)

;スペルチェック
(setq-default flyspell-mode t)

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
;; tをnilにすると部分一致
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


;バッファが同じ名前のときの表示名を指定する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;--------------------------------------------------
;key bindings
;--------------------------------------------------

;全角入力を半角に変換します。
(lexical-let
 ((words `(
  ;(,[?¥].  ,[?\\])
  ("；". ";")
  ("：".  ":")
  ("）". ")")
  ("（". "(")
  ("。". ".")
  ("＊". "*")
  ("　". " "))))
 (progn
  (defun set-key (input output)
   (global-set-key input
    `(lambda () (interactive)
     (insert ,output))))
  (dolist (w words) (set-key (car w) (cdr w)))))

(global-set-key [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する
;(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(global-set-key [f9] 'linum-mode)  ; 行番号を表示
(global-set-key "\C-cw" 'whitespace-mode)
(define-key global-map (kbd "C-l") 'anything)
(global-set-key [f12] 'flymake-goto-next-error)  ; errorへジャンプ
(global-set-key (kbd "S-<f11>") 'flymake-goto-prev-error)
(global-set-key "\C-cv" 'revert-buffer-force)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-z") 'suspend-emacs)
(global-set-key (kbd "M-V") 'toggle-vi-mode)
(global-set-key (kbd "M-<f1>") 'split-window-by-5)
;括弧の補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "M-Q") 'keyboard-escape-quit)


;--------------------------------------------------
;order-made function
;--------------------------------------------------

(defun toggle-vi-mode ()
  (interactive)
  (if (not (string= "vi-mode" major-mode))
      (vi-mode)
    (setq mode-name vi-mode-old-mode-name)
    (setq case-fold-search vi-mode-old-case-fold)
    (use-local-map vi-mode-old-local-map)
    (setq major-mode vi-mode-old-major-mode)
    (force-mode-line-update)))

(defun split-window-by-5 ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-vertically))

(defun python-insert-ipdb ()
  (interactive)
  (indent-new-comment-line)
  (insert "import ipdb; ipdb.set_trace()"))

(defun python-delete-ipdb ()
  (interactive)
  (delete-matching-lines "import ipdb; ipdb.set_trace()"))

;--------------------------------------------------
;init
;--------------------------------------------------

;window 操作
(split-window-horizontally)

