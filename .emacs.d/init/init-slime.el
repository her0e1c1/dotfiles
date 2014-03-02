(require 'slime)

;; npm install -g swank-js
;; npm install -g socket.io

(slime-setup '(slime-repl slime-fancy slime-banner slime-js))

(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

(provide 'init-slime)
