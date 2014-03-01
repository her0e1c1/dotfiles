(require 'helm-config)
(global-set-key (kbd "C-l") 'helm-mini)

; (helm :sources '(helm-source-findutils
;                  helm-source-recentf
;                  helm-source-bookmarks
;                  helm-source-buffers-list
;                  helm-source-google-suggest
;                  helm-source-locate
;                  helm-source-ls-git)
;       :buffer "*helm all the things*")
(provide 'init-helm)
