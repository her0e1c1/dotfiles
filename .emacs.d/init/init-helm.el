(require 'helm-config)
(require 'helm-files)
(require 'helm-swoop)
(require 'helm-dired-recent-dirs)

(defun helm-mini()
  (interactive)
  (require 'helm-files)
  (helm-other-buffer
        '(
        helm-source-recentf
        ;helm-source-occur
        helm-source-buffers-list
        helm-source-find-files
        ;; helm-source-findutils
        ;; helm-source-dired-recent-dirs
        helm-source-files-in-current-dir
        ;; helm-source-file-name-history
        ;; helm-source-bookmarks
        ;; helm-source-google-suggest
        ;; helm-source-locate
        ;; helm-source-mac-spotlight
        ;helm-source-ls-git
        )
      "*helm mini*"))


;; 現在のバッファに対して検索
(global-set-key (kbd "M-s") 'helm-swoop)

;; 全てのバッファに対して検索(複数のバッファが必要)
(global-set-key (kbd "M-S") 'helm-multi-swoop-all)
;(global-set-key (kbd "M-S") 'helm-multi-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; 値がtの場合はウィンドウ内に分割、nilなら別のウィンドウを使用
(setq helm-swoop-split-with-multiple-windows nil)

;; ウィンドウ分割方向 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; nilなら一覧のテキストカラーを失う代わりに、起動スピードをほんの少し上げる
(setq helm-swoop-speed-or-color t)

;(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)

;; isearch実行中にhelm-swoopに移行
;(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; helm-swoop実行中にhelm-multi-swoop-allに移行
;(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

; mini バッファを表示
(global-set-key (kbd "C-l") 'helm-mini)

; 最近開いたファイルを表示 
(global-set-key (kbd "C-x C-r") 'helm-recentf)

; 最近開いたディレクトリを表示 
(global-set-key (kbd "C-x C-d") 'helm-dired-recent-dirs-view)

; kill ringを表示
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

; find-files
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
; アクションではなくてTab補完できるようにする
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;
(define-key global-map (kbd "C-x i")   'helm-imenu)

; bufferを表示
(define-key global-map (kbd "C-x b")   'helm-buffers-list)

; M-xをhelmで実行
(global-set-key (kbd "M-x") 'helm-M-x)

; grep (markはC-@でできる)
(global-set-key (kbd "M-g") 'helm-do-grep)
;(global-set-key (kbd "M-h") 'helm-occur)

; C-hを削除キーにする
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; Tab補完をするように変更
;(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; directory nameも合わせて表示する
(setq helm-ff-toggle-basename t)
(setq helm-ff-transformer-show-only-basename nil)

(setq helm-split-window-default-side 'right)

; yasnippetで複数選択肢がある場合にhelmを起動
(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
    (interactive)
    (setq display-fn (or display-fn 'identity))
    (if (require 'helm-config)
        (let (tmpsource cands result rmap)
          (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
          (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
          (setq tmpsource
                (list
                 (cons 'name prompt)
                 (cons 'candidates cands)
                 '(action . (("Expand" . (lambda (selection) selection))))
                 ))
          (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
          (if (null result)
              (signal 'quit "user quit!")
            (cdr (assoc result rmap))))
      nil))

(setq yas-prompt-functions '(shk-yas/helm-prompt))

(provide 'init-helm)
