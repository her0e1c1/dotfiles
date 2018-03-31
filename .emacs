;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/installing-package-list
  '(
    madhat2r-theme
    s
    f
    ht
    dash
    dash-functional
    ;;; mode
    js2-mode
    typescript-mode
    web-mode
    go-mode
    python-mode
    php-mode
    scala-mode
    haskell-mode
    ; erlang
    elixir-mode dockerfile-mode
    jinja2-mode
    markdown-mode
    scss-mode
    yaml-mode
    terraform-mode
    ;;; evil
    evil
    evil-magit
    evil-ediff
    ace-jump-mode
    evil-numbers
    ;;; helm
    helm
    helm-ag
    helm-descbinds
    helm-swoop
    helm-dired-recent-dirs
    ;;; misc
    magit
    auto-complete
    ; dired+
    recentf-ext
    rainbow-delimiters
    ;;; theme
    cyberpunk-theme
    ; zerodark-theme
    ))
(defvar my/package-archives
    '(("melpa" . "http://melpa.milkbox.net/packages/")
      ("gnu" . "http://elpa.gnu.org/packages/")
      ;("marmalade" . "http://marmalade-repo.org/packages/")
	  ))
(defvar my/dired-omit-files "\\.pyc$|a.out")
(defvar my/rainbow-delimiters-languages '(scheme emacs-lisp lisp python))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (>= emacs-major-version 24)
  (require 'package)
  (dolist (pa my/package-archives)
    (add-to-list 'package-archives pa))
  (package-initialize)
  (dolist (pkg my/installing-package-list)
    (if (package-installed-p pkg)
        (require pkg)
      (package-refresh-contents)
      (package-install pkg))))

(require 'dired-x)
(require 'uniquify)

;;; Theme
(load-theme 'zerodark t)
(font-lock-mode 1)
(set-face-foreground 'font-lock-comment-face "#1C1C1C")
(set-face-background 'font-lock-comment-face "white")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-mode 1)

;; stateによって色を変更する
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
; fix escape key
(define-key key-translation-map (kbd "C-c") 'my/esc)
(define-key evil-operator-state-map (kbd "C-j") 'keyboard-quit)
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
; evil-numbers
(define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt)
; mics
(global-set-key (kbd "M-d") 'dired-jump)
(global-set-key (kbd "C-h") 'delete-backward-char)
(defalias 'exit 'save-buffer-kill-emacs)

; helpのprefix keyを変更
; M-H bとすれば、現在のmodeのbindingsを表示する(keyを忘れたり、よくわかんないmodeの使い方わかる)
; M-H fとすれば、登録関数のhelp出る
(define-key global-map "\M-H" 'help-command)
(fset 'help-command help-map)

;;; Keyjack
;;; 上書きされたくないkey binds
(defun my/toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(setq-default truncate-partial-width-windows nil)

(defun my/toggle-magit-blame-mode ()
  (interactive)
  (if magit-blame-mode
      (magit-blame-quit)
    (magit-blame
     (or magit-buffer-refname magit-buffer-revision)
     (magit-file-relative-name nil 'tracked))))

(defun my/toggle-evil-mode()
  (interactive)
  (evil-mode 'toggle))

(defvar my/current-major-mode nil)
(defun my/toggle-clipboard-mode()
  "Need to paste from clipboard. Currently it's just text-mode"
  (interactive)
  (if (eq major-mode 'text-mode)
      (if my/current-major-mode
          (funcall my/current-major-mode))
    (progn (setq my/current-major-mode major-mode) (text-mode))))

(setq my-keyjack-mode-map (make-sparse-keymap))
(mapcar
 (lambda (x)
   (define-key my-keyjack-mode-map (car x) (cdr x)))
 `(
   (,(kbd "M-a M-l") . my/toggle-truncate-lines)
   (,(kbd "M-a M-c") . my/toggle-clipboard-mode)
   (,(kbd "M-a M-a") . my/toggle-input-method)  ; 日本語入力切替
   (,(kbd "M-a M-e") . my/toggle-evil-mode)
   (,(kbd "M-a M-b") . my/toggle-magit-blame-mode)
   (,(kbd "M-a M-n") . linum-mode)
   (,(kbd "M-a M-w") . whitespace-mode)
   (,(kbd "M-a M-m") . magit-status)
   (,(kbd "M-a M-z") . evil-emacs-state)
   (,(kbd "M-a M-d") . vc-diff)
))

(easy-mmode-define-minor-mode
 my-keyjack-mode-map
 "KeyJack"
 t
 " KJ"
 my-keyjack-mode-map
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-split-window-default-side 'right)
(custom-set-variables
 '(helm-mini-default-sources '(helm-source-buffers-list
   helm-source-files-in-current-dir
   helm-source-recentf
   helm-source-buffer-not-found
   )))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-b") 'helm-buffers-list)
(global-set-key (kbd "M-m") 'helm-mini)
(global-set-key (kbd "M-s") 'helm-swoop)
(global-set-key (kbd "M-k") 'helm-descbinds)
(global-set-key (kbd "M-f") 'helm-ag)
(define-key helm-map (kbd "C-h") 'delete-backward-char)  ; C-hを削除キーにする

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Python
(defun python-insert-ipdb ()
  (interactive)
  (indent-new-comment-line)
  (insert "import pdb; pdb.set_trace()"))

(add-hook 'python-mode-hook
 '(lambda () (define-key python-mode-map (kbd "C-c C-i") 'python-insert-ipdb)
   ))

;;; ts(x)
; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

;;; html, js(x)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (interactive)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;;; Haskell
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;;; rainbow-delimiters
; タイミングが早いと上手くロードされないみたい？
(dolist (x my/rainbow-delimiters-languages)
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook"))
   'rainbow-delimiters-mode))

;;; recentf
(recentf-mode 1)
(setq recentf-auto-cleanup 'never)
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(setq recentf-max-menu-items  1000)
(setq recentf-max-saved-items 1000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH" (concat (getenv "PATH") ":/bin"))
(setq exec-path (append exec-path '("/bin")))

;;;; lang
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)

;;; tab
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; scroll
; Make scroll smooth
(setq next-screen-context-lines 1)
;スクロールを一行ずつにする
(setq scroll-step 1)
(setq scroll-conservatively 1)

;dont make #file
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq backup-inhibited t)  ;バックアップファイルを作成しない

;行と列番号を表示する
(column-number-mode t)
(line-number-mode t)

;括弧にカソールを合わせたときに対応関係を表示
(show-paren-mode)

;実行できるスクリプトは保存時に実行権限をつける
(add-hook
 'after-save-hook
 'executable-make-buffer-file-executable-if-script-p)

;メニューバーを隠す
(menu-bar-mode -1)

;補完可能なものを随時表示
(icomplete-mode 0)

;"yes or no"を"y or n"に
(fset 'yes-or-no-p 'y-or-n-p)

;リージョンを kill-ring に入れないで削除できるようにする
(delete-selection-mode t)

; Hide specified files in dired
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files  my/dired-omit-files)

;buffer再度読み込みをします。
(global-auto-revert-mode 1)

;バッファが同じ名前のときの表示名を指定する
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;beep音を消す
(setq ring-bell-function 'ignore)

;補完可能なものを随時表示
(icomplete-mode 0)

;現在行を目立たせる
(global-hl-line-mode)

(setq electric-pair-preserve-balance nil)

(message "DONE :D")
