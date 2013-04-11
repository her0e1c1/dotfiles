;環境変数
;(setq exec-path (cons (expand-file-name "~/dropbox/bin") exec-path))
;(setenv "PATH" (concat (expand-file-name "~/dropbox/bin:") (getenv "PATH")))


;termの文字化け
(setq locale-coding-system 'utf-8)
(setenv "LANG" "ja_JP.UTF-8")

;再帰的にパスを加えて行く
(let ((default-directory (expand-file-name "~/.emacs.d/")))
  (add-to-list 'load-path default-directory)
   (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
       (normal-top-level-add-subdirs-to-load-path)))

(require 'auto-install)
(setq auto-install-directory "~/dropbox/conf/emacs/auto-install/")
;(auto-install-update-emacswiki-package-name t)
;(auto-install-compatibility-setup)

;--------------------------------------------------
;face
;--------------------------------------------------

(add-hook 'rst-mode-hook 
          (lambda ()
            ;(setq rst-level-face-base-color "selectedKnobColor")
            (set-face-background 'rst-level-1-color "selectedKnobColor")
            ))

;--------------------------------------------------
;拡張子
;--------------------------------------------------
;; (let (mode)
;;   (if (require 'python-mode nil t)
;;       (setq mode 'python-mode)
;;     (setq mode 'python))
;;   (add-to-list 'auto-mode-alist `("\\.py\\'" . ,mode)))

(add-to-list 'auto-mode-alist `("\\.py\\'" . python-mode))


;--------------------------------------------------
;rst
;--------------------------------------------------

;(setcar (cdr(assq 's5 rst-compile-toolsets)) "rst2s5.py")
(setq rst-slides-program "open -a opera")


;--------------------------------------------------
;anything
;--------------------------------------------------

(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

;設定
(setq kill-ring-max 30)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200 )

(setq anything-sources 
 '(anything-c-source-buffers+ 
 anything-c-source-file-name-history 
 anything-c-source-recentf
 anything-c-source-bookmarks
 anything-c-source-register
 anything-c-source-kill-ring

 anything-c-source-buffer-not-found 
 anything-c-source-imenu 
 anything-c-source-etags-select
 anything-c-source-files-in-current-dir
 anything-c-source-man-pages
 anything-c-source-emacs-commands
 anything-c-source-emacs-functions
 
 ;self made
 anything-c-source-plocate 
 anything-c-source-home-locate
 ))
;; 自動でimenuのインデックスを作る 
(setq imenu-auto-rescan t)

(defvar anything-c-source-home-locate 
 '((name . "Home Locate") 
 (candidates . (lambda () 
 (apply 'start-process "anything-home-locate-process" nil 
 (home-locate-make-command-line anything-pattern "-r")))) 
 (type . file) 
 (requires-pattern . 3) 
 (delayed)))

(defvar anything-c-source-plocate 
 '((name . "Project Locate") 
 (candidates 
 . (lambda () 
 (let ((default-directory 
 (with-current-buffer anything-current-buffer default-directory))) 
 (apply 'start-process "anything-plocate-process" nil 
 (plocate-make-command-line anything-pattern "-r"))))) 
 (type . file) 
 (requires-pattern . 3) 
 (delayed)))

;--------------------------------------------------
;OSごとの環境設定
;--------------------------------------------------
;mac
(if (or (eq window-system 'mac)
        (eq window-system 'ns))
    (set-frame-font "Hiragino-14")
    (let ()
	  ;フォントサイズを変更する
      ;(require 'carbon-font)
      ;(fixed-width-set-fontset "hirakaku_w3" 10)
      )
  (set-default-font "Monospace-12")
  )


(setq inferior-lisp-program "ccl")
;--------------------------------------------------
;lisp
;--------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))

;--------------------------------------------------
;各種設定
;--------------------------------------------------


;; 日本語化
(prefer-coding-system 'utf-8)

;C-x C-fで大文字小文字をを区別しない
(setq completion-ignore-case t)

;eval-expression M-:でtab補完する
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;行と列番号を表示する
(column-number-mode t)
(line-number-mode t)

;schemeの処理系を入れる M-x run-scheme
(setq scheme-program-name "/opt/local/bin/gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(setq inhibit-startup-message t) ;when stating, dont split a display into two
(setq backup-inhibited t) ;dont make backup file
;(set-frame-parameter nil 'fullscreen 'maximized);サイズの変更

;括弧の対応関係を表示
(show-paren-mode)

;dont make #file
(setq auto-save-default nil)
(setq make-backup-files nil)

;delete tool bar 
(tool-bar-mode 0)

;delete で選択範囲を削除する、この時kill ringに値を格納しない
(delete-selection-mode t)

;他のアプリケーションでの貼り付け可能にする
(setq x-select-enable-clipboard t)

;;; 補完可能なものを随時表示
(icomplete-mode 0)

;; C-kで行全体を削除
(setq kill-whole-line t)

;; "yes or no"を"y or n"に
(fset 'yes-or-no-p 'y-or-n-p)

;括弧の補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)

;bufferの切り替えを楽にする
(iswitchb-mode 1)
;; tをnilにすると部分一致
(setq iswitchb-regexp t)

;beep音を消す
(setq ring-bell-function 'ignore)

;スクロールバーを消す
(toggle-scroll-bar nil)

;スクロールを一行ずつにする
(setq scroll-step 1)
(setq scroll-conservatively 1)

;;; 現在行を目立たせる
(global-hl-line-mode)

;時計の表示
;(display-time-mode 1)

;;; 最終行に必ず一行挿入する
;(setq require-final-newline t)


;スクロールで画面を少し残す
(setq next-screen-context-lines 1)

;tab幅
;(setq mac-command-key-is-meta t)
(put 'downcase-region 'disabled nil)
;; Macのキーバインドを使う。optionをメタキーにする。
;(mac-key-mode 1)
;(setq mac-option-modifier 'meta)
;key bindが変わった場合
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;window切り替えをshift+arrowで可能にする
(windmove-default-keybindings)
;巡回する
(setq windmove-wrap-around t)

;window 操作
(split-window-horizontally)
;(split-window-horizontally)
;(other-window 1)
;(split-window-vertically)

;window
(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "Gray")
(add-to-list 'default-frame-alist '(alpha . (0.95 0.90)))

;C-RETで、矩形操作をする。
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止

;tab
;; (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
;;                       64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
;M-4 C-x tab もまとめたい
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(when window-system 
    (let (
          (px (display-pixel-width)) 
          (py (display-pixel-height)) 
          (fx (frame-char-width)) 
          (fy (frame-char-height)) 
          tx ty 
          ) 
      ;; Next formulas discovered empiric on Windows/Linux host 
      ;; with default font (7x13). 
      (setq tx (- (/ px fx) 7)) 
      (setq ty (- (/ py fy) 4)) 
      (setq initial-frame-alist '((top . 2) (left . 2))) 
      (add-to-list 'initial-frame-alist (cons 'width tx)) 
      (add-to-list 'initial-frame-alist (cons 'height ty))))

;実行権限を自動で付ける
(add-hook 'after-save-hook
		  '(lambda ()
             (save-restriction
               (widen)
               (if (string= "#!" (buffer-substring 1 (min 3 (point-max))))
                   (let ((name (buffer-file-name)))
                     (or (char-equal ?. (string-to-char (file-name-nondirectory name)))
                         (let ((mode (file-modes name)))
                           (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                           (message (concat "Wrote " name " (+x)"))))
                     )))))
;起動
;(anything)

;--------------------------------------------------
;                    python
;--------------------------------------------------

(add-hook 'python-mode-hook 
		  '(lambda ()
             (require 'python-mode)
             (define-key python-mode-map (kbd "C-c C-i") 'python-insert-ipdb)
             (define-key python-mode-map (kbd "C-c C-d") 'python-delete-ipdb)
             ;(require 'flymake)
             (setq ipython-completion-command-string  "print(';'.join(get_ipython().complete('%s', '%s')[1])) #PYTHON-MODE SILENT\n")
             (setq py-shell-name "ipython3")
             (setq ipython-command "/Users/air/python/3.3.0/bin/ipython3")
             ;(require 'ipython)
			 (add-hook 'find-file-hook 'flymake-find-file-hook)
			 (when (load "flymake" t)
			   (defun flymake-pyflakes-init ()
				 (let* ((temp-file (flymake-init-create-temp-buffer-copy
									'flymake-create-temp-inplace))
						(local-file (file-relative-name
									 temp-file
									 (file-name-directory buffer-file-name))))
				   (list "pycheckers"  (list local-file))))
			   (add-to-list 'flymake-allowed-file-name-masks
							'("\\.py\\'" flymake-pyflakes-init)))
			 
			 (load-library "flymake-cursor")
			 ;(define-key python-mode-map (kbd "C-c i") 'py-insert-ipdb)
			 ))

;python mode
(setq py-python-command "/Users/air/python/3.3.0/bin/ipython3")

;--------------------------------------------------
;key bindings
;--------------------------------------------------
(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(setq-default truncate-partial-width-windows nil)

;; revert-buffer without asking
;;python-modeなどでは無効になってる
(defun revert-buffer-force()
  (interactive)
  (revert-buffer nil t)
)


;; \C-aでインデントを飛ばした行頭に移動
(defun beginning-of-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。"
  (interactive "d")
  (if (string-match
       "^[ ¥t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun other-window-backward ()
  (interactive)
  (other-window -1))

;;上書きされたくないkey binds
(setq my-keyjack-mode-map (make-sparse-keymap))
(mapcar (lambda (x)
		  (define-key my-keyjack-mode-map (car x) (cdr x))
		  ;(global-set-key (car x) (cdr x))
		  )
		`(("\C-t" . other-window)
		  (,(kbd "C-S-t") . other-window-backward)
		  ("\C-c\C-l" . toggle-truncate-lines)
		  (,(kbd "M-g") . goto-line)
          (,(kbd "C-z") . undo )
		  ;("\C-a" . beginning-of-indented-line)  ;M-m
		  ))


(easy-mmode-define-minor-mode my-keyjack-mode-map "Grab Keys"
							  t " KeyJack" my-keyjack-mode-map)

;全角スペースを半角スペースにして入力します。
;(global-set-key "　" (lambda () (interactive)(insert " ")))
;12288は全角スペース
;(global-set-key [?\S-　] '(lambda () (interactive)(insert 12288)))
(define-key global-map [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する
;(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(global-set-key [f9] 'linum-mode)  ; 行番号を表示
(global-set-key "\C-cw" 'whitespace-mode)
(define-key global-map (kbd "C-l") 'anything)
(global-set-key [f12] 'flymake-goto-next-error)  ; errorへジャンプ
(global-set-key (kbd "S-<f11>") 'flymake-goto-prev-error)
(global-set-key "\C-cv" 'revert-buffer-force)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)
;(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-V") 'toggle-vi-mode)
(global-set-key (kbd "M-<f1>") 'split-window-by-5)

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
  ;(keyboard-escape-quit) Esc Esc Esc
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-vertically)
  ;(other-window 2)
  ;(split-window-vertically)
  )

;pointを表示
(add-to-list 'mode-line-format '(:eval (int-to-string (point))))

;全角記号を半角にする；：＜＞（）『』「」など


(defun python-insert-ipdb ()
  (interactive)
  (indent-new-comment-line)
  (insert "import ipdb; ipdb.set_trace()"))

(defun python-delete-ipdb ()
  (interactive)
  (delete-matching-lines "import ipdb; ipdb.set_trace()"))

(load "~/.emacs.d/haskell-mode/haskell-site-file")
