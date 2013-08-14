(defvar temp-directory (concat "~/tmp/" (user-login-name)))
(make-directory temp-directory t)

; One of the main issues for me is that my home directory is
; NFS mounted.  By setting all the autosave directories in /tmp,
; things run much quicker
(setq auto-save-directory (concat temp-directory "/autosave")
      auto-save-hash-directory (concat temp-directory "/autosave-hash")
      auto-save-directory-fallback "/tmp/"
      auto-save-list-file-prefix (concat temp-directory "/autosave-")
      auto-save-hash-p nil
      auto-save-timeout 100
      auto-save-interval 300)
(make-directory auto-save-directory t)
;(require 'auto-save)

(defadvice recentf-open-files (after recentf-set-overlay-directory-adv activate)
  (set-buffer "*Open Recent*")
  (save-excursion
    (while (re-search-forward "\\(^  \\[[0-9]\\] \\|^  \\)\\(.*/\\)$" nil t nil)
      (overlay-put (make-overlay (match-beginning 2) (match-end 2))
                   'face `((:foreground ,"#F1266F"))))))
;(require 'recentf-ext)
(setq load-path (append (list (expand-file-name "~/.icalendar")) load-path))
(require 'icalendar)
(setq appt-display-duration 60)


;--------------------------------------------------;
;文字コードの設定
;--------------------------------------------------

;termの文字化け
(setq locale-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(setenv "LANG" "ja_JP.UTF-8")

;日本語化
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

;初期位置
(cd "~/")

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

;dont make backup file
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

;emacsを終了した時の状態を保存

;; (setq desktop-dirname "~/")
;; (setq desktop-base-file-name ".emacs.desktop")
;; (setq desktop-save t)
;; (require 'desktop)
;; (desktop-save-mode 1)
;; (desktop-load-default)
;; (desktop-read)
;; (setq history-length 250)
;; (setq desktop-dirname "~/")
;; (add-to-list 'desktop-globals-to-save 'extended-command-history)
;; (add-to-list 'desktop-globals-to-save 'shell-command-history)
;; (defadvice save-buffers-kill-terminal
;;   (before deskto-save-before-kill-emacs activate)
;;   (setq desktop-save t)
;;   (setq desktop-base-file-name ".emacs.desktop")
;;   (desktop-save "~/")
;;   )
;; ;(add-to-list 'desktop-globals-to-save 'file-name-history)
;; (setq desktop-dirname "~/")
;; (setq desktop-base-file-name ".emacs.desktop")

(require 'filecache)
(file-cache-add-directory-list
   (list "~" "~/bin"))
(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (local-set-key "\C-c\C-i" 'file-cache-minibuffer-complete)))

;
(setq skeleton-pair 1)

;python
(add-to-list 'load-path "~/.emacs.d/python")
;(require 'python-mode)

;--------------------------------------------------
;standard mode
;--------------------------------------------------

;バッファが同じ名前のときの表示名を指定する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;履歴を表示
(when (require 'recentf nil t)
  (setq recentf-auto-save-timer)
  (setq recentf-max-menu-items  100000)
  (setq recentf-max-saved-items 100000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 'never)
  (run-with-idle-timer 30 t 'recentf-save-list)
  (recentf-mode 1))

(require 'flymake)
(require 'cl)


;--------------------------------------------------
;iswitch
;--------------------------------------------------

(add-hook 'iswitchb-define-mode-map-hook
      (lambda ()
        (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
        (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
        (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
        (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))
;(iswitchb-default-keybindings)
;不要なバッファは無視する
;(setq iswitchb-buffer-ignore '("^\\*"))
;(add-to-list 'iswitchb-buffer-ignore "/\/i/\/i`/\/i/\/i*")

;--------------------------------------------------
;order-made function
;--------------------------------------------------

;; (defadvice yes-or-no-p (around yes-or-no-p-always-yes)
;;   "Return always yes."
;;   (setq ad-return-value t))
;; (ad-activate-regexp "yes-or-no-p-always-yes")
;; (ad-deactivate-regexp "yes-or-no-p-always-yes")

;scrachの中身を終了時に保存
(setq scratch-path "~/.scratch")
(defun save-scratch-data ()
  (let ((str (progn
               (set-buffer (get-buffer "*scratch*"))
               (buffer-substring-no-properties
                (point-min) (point-max))))
        (file scratch-path))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert str)
    (save-buffer)
    (kill-buffer buf)))
;scrathの中身を開始時に復元
(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))
(defadvice kill-emacs
  (before save-scratch-buffer1 activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file scratch-path))
    (when (file-exists-p file)
      (set-buffer (get-buffer "*scratch*"))
      (erase-buffer)
      (insert-file-contents file))
    ))
(read-scratch-data)

(defun indent-rigidly-4 (beg end &optional spaces)
  "`indent-rigidly' 4 spaces.
With prefix-arg, or optional arg SPACES, `indent-rigidly' by that amount
instead."
  (interactive "r\nP")
  (let* ((value (prefix-numeric-value spaces))
        (default-width 4)
        (width default-width))
    (cond ((= value -1) (setq width (* -1 default-width)))
          ((null spaces) (setq width default-width))
          (t (setq width value)))
    (indent-rigidly beg end width)))

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
;KeyJack mode
;--------------------------------------------------

(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(setq-default truncate-partial-width-windows nil)

(defun other-window-backward ()
  (interactive)
  (other-window -1))

;;上書きされたくないkey binds
(setq my-keyjack-mode-map (make-sparse-keymap))
(mapcar
 (lambda (x)
  (define-key my-keyjack-mode-map (car x) (cdr x)))
`(;("\M-t" . other-window)
  (,(kbd "C-S-t") . other-window-backward)
  ("\C-c\C-l" . toggle-truncate-lines)
  (,(kbd "M-g") . goto-line)
  ))

(easy-mmode-define-minor-mode
 my-keyjack-mode-map "Grab Keys"
 t " KeyJack" my-keyjack-mode-map)

;--------------------------------------------------
;python-hook
;--------------------------------------------------

(add-hook 'python-mode-hook 
 '(lambda ()
    (define-key python-mode-map (kbd "C-c i") 'python-insert-ipdb)
    (define-key python-mode-map (kbd "C-c d") 'python-delete-ipdb)
   (add-hook 'find-file-hook 'flymake-find-file-hook)
   (when (load "flymake" t)
    (defun flymake-pyflakes-init ()
     (let* ((temp-file (flymake-init-create-temp-buffer-copy
			'flymake-create-temp-inplace))
			(local-file
             (file-relative-name
			 temp-file
			 (file-name-directory buffer-file-name))))
		   (list "pycheckers"  (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
     '("\\.py\\'" flymake-pyflakes-init)))
   (load-library "flymake-cursor")))

;--------------------------------------------------
;scheme
;--------------------------------------------------

(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)


;--------------------------------------------------
;key bindings
;--------------------------------------------------

;全角入力を半角に変換します。
(let
 ((words `(
  ;(,[?¥].  ,[?\\])
  ("０". "0")
  ("１". "1")
  ("２". "2")
  ("３". "3")
  ("４". "4")
  ("５". "5")
  ("６". "6")
  ("７". "7")
  ("８". "8")
  ("９". "9")
  ;("ー". "-")
  ("；". ";")
  ("：".  ":")
  ("）". ")")
  ("（". "(")
  ("。". ".")
  ("＊". "*")
  ("＜". "<")
  ("＞". ">")
  ("、". ",")
  ("＃". "#")
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
(global-set-key [f12] 'flymake-goto-next-error)  ; errorへジャンプ
(global-set-key (kbd "S-<f11>") 'flymake-goto-prev-error)
(global-set-key "\C-cv" 'revert-buffer-force)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-,") 'previous-buffer)
;(global-set-key (kbd "C-z") 'suspend-emacs)
(global-set-key (kbd "C-l") 'recentf-open-files)
(define-key global-map (kbd "C-x C-l") 'iswitchb-buffer)
(global-set-key (kbd "C-x TAB") 'indent-rigidly-4)
(global-set-key (kbd "M-V") 'toggle-vi-mode)
(global-set-key (kbd "M-<f1>") 'split-window-by-5)
;括弧の補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "M-Q") 'keyboard-escape-quit)
(global-set-key (kbd "M-G") 'grep-find)


;--------------------------------------------------
;init
;--------------------------------------------------

;window 操作
;(split-window-horizontally)
(put 'upcase-region 'disabled nil)


(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "M-t") 'other-window-or-split)

;; ブックマークを変更したら即保存する
(setq bookmark-save-flag 1)
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest ( bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-aliset))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))

(require 'server)

(defvar installing-package-list
  '(
    ;w3m
    icalendar
    ;php-mode
    ;scala-mode
    ;markdown-mode
    ;scss-mode
    ;haskell-mode
    ;google-c-style
    ;yaml-mode
    ;open-junk-file
    bookmark+
    recentf-ext
    dired+
    ))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (unless (server-running-p)
    (package-initialize))
  (dolist (pkg installing-package-list)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;emacs server
;this code should be at the last line
(unless (server-running-p)
 (server-start))

