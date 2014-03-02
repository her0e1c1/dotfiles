(require 'expand-region)

; C-@の機能を拡張したものと考える
(global-set-key (kbd "C-@") 'er/expand-region)
;(global-set-key (kbd "C-M-@") 'er/contract-region)

(provide 'init-expand-region)
