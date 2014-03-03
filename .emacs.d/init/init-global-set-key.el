; evil modeの切替
(global-set-key (kbd "M-e") 'toggle-evil-mode)

; eshellを起動
(global-set-key (kbd "M-E") 'eshell)

; ¥の代わりにバックスラッシュを入力する
;(global-set-key [?¥] [?\\])

; 行番号を表示
(global-set-key (kbd "C-c i") 'linum-mode)

; 空文字を表示
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; (global-set-key "\C-cv" 'revert-buffer-force)
;; (global-set-key (kbd "M-.") 'next-buffer)
;; (global-set-key (kbd "M-,") 'previous-buffer)
;; (global-set-key (kbd "M-k")
;;     (lambda () (interactive)(delete-region (line-beginning-position) (1+(point)))))
;; (global-set-key (kbd "M-F") 
;;     (lambda () (interactive) (message "%s"(get-text-property (point) 'face))))

(global-set-key (kbd "C-x C-o") 'find-file)
(global-set-key (kbd "C-x C-l") 'recentf-open-files)
(global-set-key (kbd "C-x <escape>") 'kill-emacs)
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

; 入力メソッドを切り換える
(global-set-key (kbd "M-a") 'toggle-input-method)

; 次のwindowに移動する
(global-set-key (kbd "M-t") 'other-window-or-split)
(provide 'init-global-set-key)
