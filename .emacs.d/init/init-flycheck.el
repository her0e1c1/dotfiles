; flycheck
;$ sudo easy-install flake8  # インストール必須
(add-hook 'python-mode-hook 'flycheck-mode)

(provide 'init-flycheck)
