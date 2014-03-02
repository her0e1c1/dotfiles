(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(setq-default truncate-partial-width-windows nil)

(defun other-window-backward ()
  (interactive)
  (other-window -1))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;;上書きされたくないkey binds
(setq my-keyjack-mode-map (make-sparse-keymap))
(mapcar
 (lambda (x)
  (define-key my-keyjack-mode-map (car x) (cdr x)))
`(
  ;("\M-t" . other-window)
  (,(kbd "C-S-t") . other-window-backward)
  ;("\C-c\C-l" . toggle-truncate-lines)
  ("\C-cl" . toggle-truncate-lines)
  ;(,(kbd "M-g") . goto-line)
  ))

(easy-mmode-define-minor-mode
 my-keyjack-mode-map "Grab Keys"
 t " KeyJack" my-keyjack-mode-map)

(provide 'init-keyjack)
