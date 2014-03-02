(require 'yasnippet)
(setq yas-snippet-dirs
  '("~/.emacs.d/snippets"
    ))

;; gitからスニペットを取得する
(defvar git-yasnippet-snippets "~/.emacs.d/snippets/yasnippet-snippets")
(unless (f-dir? git-yasnippet-snippets)
  (when (shell-command
         (format "git clone https://github.com/AndreaCrotti/yasnippet-snippets.git %s"
                 git-yasnippet-snippets))
    (message (format "can't clone the snippet repogitory at %s" git-yasnippet-snippets))))
(add-to-list 'yas/root-directory git-yasnippet-snippets)

(yas-global-mode 1)

;; use "M-o" to expand, not "TAB"
(define-key yas-minor-mode-map (kbd "M-o") 'yas-expand)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(provide 'init-yasnippet)
