(global-set-key (kbd "M-e") 'toggle-evil-mode)
(global-set-key (kbd "M-E") 'eshell)
(global-set-key [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する
;(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(global-set-key (kbd "C-c i") 'linum-mode)  ; 行番号を表示
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key [f12] 'flymake-goto-next-error)  ; errorへジャンプ
(global-set-key (kbd "S-<f11>") 'flymake-goto-prev-error)
;; (global-set-key "\C-cv" 'revert-buffer-force)
;; (global-set-key (kbd "M-.") 'next-buffer)
;; (global-set-key (kbd "M-,") 'previous-buffer)
;; (global-set-key (kbd "M-k")
;;     (lambda () (interactive)(delete-region (line-beginning-position) (1+(point)))))
;; (global-set-key (kbd "M-F") 
;;     (lambda () (interactive) (message "%s"(get-text-property (point) 'face))))

;(global-set-key (kbd "C-l") 'recentf-open-files)
(define-key global-map (kbd "C-x C-l") 'iswitchb-buffer)
(global-set-key (kbd "C-x TAB") 'indent-rigidly-4)
(global-set-key (kbd "M-V") 'toggle-vi-mode)
(global-set-key (kbd "M-<f1>") 'split-window-by-5)
(global-set-key (kbd "C-S-D") 'vc-diff)

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

;; (global-set-key (kbd "M-G") 'grep-find)

;; but [escape] should switch back to normal state
(global-set-key (kbd "M-a") 'toggle-input-method)

(global-set-key (kbd "M-t") 'other-window-or-split)
(provide 'init-global-set-key)
