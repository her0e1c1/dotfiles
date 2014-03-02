
; ターミナルにバインドに特殊文字を送る
(defun event-apply-control-shift-modifier (ignore-prompt)
  "\\Add the Control+Shift modifier to the following event.
For example, type \\[event-apply-control-shift-modifier] SPC to enter Control-Shift-SPC."
  (vector (event-apply-modifier
           (event-apply-modifier (read-event) 'shift 25 "S-")
           'control 26 "C-")))

(define-key function-key-map (kbd "C-x @ C") 'event-apply-control-shift-modifier)
