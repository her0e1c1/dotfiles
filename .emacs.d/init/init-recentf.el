;履歴を表示
(when (require 'recentf nil t)
  (setq recentf-auto-save-timer)
  (setq recentf-max-menu-items  1000)
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 'never)
  (run-with-idle-timer 60 t 'recentf-save-list)
  (recentf-mode 1))

(provide 'init-recentf)
