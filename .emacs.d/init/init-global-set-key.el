; evil modeの切替
(global-set-key (kbd "M-e") 'toggle-evil-mode)

; eshellを起動
(global-set-key (kbd "M-E") 'eshell)

; helpのprefix keyを変更
(define-key global-map "\M-H" 'help-command)
(fset 'help-command help-map)

; ¥の代わりにバックスラッシュを入力する
;(global-set-key [?¥] [?\\])

;; (global-set-key "\C-cv" 'revert-buffer-force)
;; (global-set-key (kbd "M-.") 'next-buffer)
;; (global-set-key (kbd "M-,") 'previous-buffer)
;; (global-set-key (kbd "M-k")
;;     (lambda () (interactive)(delete-region (line-beginning-position) (1+(point)))))
;; (global-set-key (kbd "M-F") 
;;     (lambda () (interactive) (message "%s"(get-text-property (point) 'face))))

(global-set-key (kbd "C-x C-o") 'find-file)
(global-set-key (kbd "C-x C-l") 'recentf-open-files)
(global-set-key (kbd "C-x <escape>") 'save-buffers-kill-terminal)
;; (define-key global-map (kbd "C-x C-l") 'iswitchb-buffer)
(global-set-key (kbd "C-x TAB") 'indent-rigidly-4)
;; (global-set-key (kbd "M-V") 'toggle-vi-mode)
;; (global-set-key (kbd "M-<f1>") 'split-window-by-5)
;; (global-set-key (kbd "C-S-D") 'vc-diff)

; バックスペースを設定
(global-set-key (kbd "C-h") 'delete-backward-char)

; windowの分割を取り消す
(global-set-key (kbd "M-Q") 'keyboard-escape-quit)

; 正規表現の確認にre-builderを使う
(global-set-key (kbd "M-R") 're-builder)

;; ;括弧の補完
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

; find-grep を実行する
(global-set-key (kbd "M-F") 'grep-find)

;; but [escape] should switch back to normal state

; 次のwindowに移動する
(global-set-key (kbd "M-t") 'other-window-or-split)

; スペ-ス4でインデントする
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

(global-set-key (kbd "C-x TAB") 'indent-rigidly-4)

(provide 'init-global-set-key)
