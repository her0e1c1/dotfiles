(defun indent-rigidly-4 (beg end &optional spaces)
  "`indent-rigidly' 4 spaces.
With prefix-arg, or optional arg SPACES, `indent-rigidly' by that amount
instead."
  (interactive "r\nP")
  (let* ((value (prefix-numeric-value spaces))
        (default-width 4)
        (width default-width))
    (cond ((= value -1) (setq width (* -1 default-width)))
          ((null spaces) (setq width default-width))
          (t (setq width value)))
    (indent-rigidly beg end width)))

(defun toggle-vi-mode ()
  (interactive)
  (if (not (string= "vi-mode" major-mode))
      (vi-mode)
    (setq mode-name vi-mode-old-mode-name)
    (setq case-fold-search vi-mode-old-case-fold)
    (use-local-map vi-mode-old-local-map)
    (setq major-mode vi-mode-old-major-mode)
    (force-mode-line-update)))

(defun split-window-by-5 ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-vertically))
