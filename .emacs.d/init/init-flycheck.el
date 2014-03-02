;$ sudo easy-install flake8 pylint  # インストール必須
(unless (executable-find "flake8")
  (with-temp-buffer 
    (cd "/sudo::/")
    (shell-command "sudo easy-install flake8")))

;; flymake-cursor
(global-set-key (kbd "M-N") 'flycheck-next-error)
(global-set-key (kbd "M-P") 'flycheck-previous-error)

(add-hook 'python-mode-hook 'flycheck-mode)

(provide 'init-flycheck)
