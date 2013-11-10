;--------------------------------------------------
;packages
;--------------------------------------------------

(defvar installing-package-list
  '(
    ;w3m
    ;icalendar
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
    js2-mode
    evil
    key-chord
    ))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  ;(package-refresh-contents)
  (dolist (pkg installing-package-list)
    (if (package-installed-p pkg)
        (require pkg)
      (package-refresh-contents)
      (package-install pkg))))


(require 'evil)
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)
;--------------------------------------------------
;js
;--------------------------------------------------

(global-set-key (kbd "C-c C-c") 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.tpl$" . jinja2-mode))

;--------------------------------------------------
;rst
;--------------------------------------------------

(add-hook 'rst-mode-hook
 '(lambda ()
 (define-key rst-mode-map (kbd "C-c C-n") 'rst-forward-section)
 (define-key rst-mode-map (kbd "C-c C-p") 'rst-backward-section)
 (define-key rst-mode-map (kbd "C-c C-t") 'rst-toc)))

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
  ("｀". "`")
  ("：".  ":")
  ("）". ")")
  ("（". "(")
  ("。". ".")
  ("＊". "*")
  ("＜". "<")
  ("＠", "@")
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

(defun toggle-evil-mode()
  (interactive)
  (evil-mode 'toggle))

(global-set-key (kbd "M-e") 'toggle-evil-mode)
(global-set-key [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する
;(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(global-set-key (kbd "C-c C-i") 'linum-mode)  ; 行番号を表示
(global-set-key "\C-cw" 'whitespace-mode)
(global-set-key [f12] 'flymake-goto-next-error)  ; errorへジャンプ
(global-set-key (kbd "S-<f11>") 'flymake-goto-prev-error)
(global-set-key "\C-cv" 'revert-buffer-force)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-,") 'previous-buffer)
(global-set-key (kbd "M-k")
    (lambda () (interactive)(delete-region (line-beginning-position) (1+(point)))))
(global-set-key (kbd "M-F") 
    (lambda () (interactive) (message "%s"(get-text-property (point) 'face))))

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

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                  (face-foreground 'mode-line))))
 (add-hook 'post-command-hook
   (lambda ()
     (let ((color (cond ((minibufferp) default-color)
                        ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                        ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                        ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                        (t default-color))))
       (set-face-background 'mode-line (car color))
       (set-face-foreground 'mode-line (cdr color))))))
(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "M-c") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "M-c") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "M-c") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "M-c") 'evil-normal-state)
(global-set-key (kbd "M-a") 'toggle-input-method)
;; (define-key help-mode-map (kbd "i") 'evil-emacs-state)
;; (define-key grep-mode-map (kbd "i") 'evil-emacs-state)
;--------------------------------------------------
;init
;--------------------------------------------------
;window 操作
;(split-window-horizontally)
;(put 'upcase-region 'disabled nil)


(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "M-t") 'other-window-or-split)

(require 'server)
(server-start)

;cl
(defmacro mac (expr)
  `(print (macroexpand ',expr)))

(defmacro nil!(x)
  `(setf ,x nil))


(defvar list-methods-mode-map nil)
(unless list-methods-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'bury-buffer)
    (define-key map "\r" 'list-methods-select-method)
    (setq list-methods-mode-map map)))

(defun list-methods-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map list-methods-mode-map)
  (setq major-mode 'list-methods-mode)
  (setq mode-name "List Methods")
  (run-hooks 'list-methods-mode-hook))

(defun list-methods-get-methods (buffer)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((case-fold-search nil)
            methods)
      (while (re-search-forward "\\(class\\|def\\) +\\([a-zA-Z0-9_]+\\)(" nil t)
        ;; バッファと位置も返すように変更
        (push (list (match-string 2) buffer (point))
                    methods))
      (nreverse methods))))

(defun list-methods ()
  (interactive)
  (let ((methods (list-methods-get-methods (current-buffer)))
        (orig-buffer (current-buffer))
        (buffer (get-buffer-create "*test*")))
    (set-buffer buffer)
    (erase-buffer)
    (dolist (method methods)
      (let ((pos (point)))
        (insert (car method) "\n")
        (put-text-property pos (point)
                              'list-methods-method-buffer (nth 1 method))
        (put-text-property pos (point)
                              'list-methods-method-position (nth 2 method))))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    ;; モード設定を追加
    (list-methods-mode)))

(defun list-methods-select-method ()
  (interactive)
  (let ((buffer (get-text-property (point) 'list-methods-method-buffer))
        (pos (get-text-property (point) 'list-methods-method-position)))
    (when (and buffer pos)
      (pop-to-buffer buffer)
      (goto-char pos))))
(put 'erase-buffer 'disabled nil)
