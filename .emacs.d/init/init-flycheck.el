;$ sudo easy-install flake8 pylint  # インストール必須
(unless (executable-find "flake8")
  (with-temp-buffer 
    (cd "/sudo::/")
    (shell-command "sudo easy-install flake8")))

(add-hook 'python-mode-hook 'flycheck-mode)

(provide 'init-flycheck)
