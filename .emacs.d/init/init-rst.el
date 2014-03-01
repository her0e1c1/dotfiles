(add-hook 'rst-mode-hook
 '(lambda ()
 (define-key rst-mode-map (kbd "C-c C-n") 'rst-forward-section)
 (define-key rst-mode-map (kbd "C-c C-p") 'rst-backward-section)
 (define-key rst-mode-map (kbd "C-c C-t") 'rst-toc)))
