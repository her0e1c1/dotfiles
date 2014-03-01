(require 'evil)

(defun toggle-evil-mode()
  (interactive)
  (evil-mode 'toggle))

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                  (face-foreground 'mode-line))))
 (add-hook 'post-command-hook
   (lambda ()
     (let ((color (cond ((minibufferp) default-color)
                        ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                        ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                        ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                        (t default-color))))
       (set-face-background 'mode-line (car color))
       (set-face-foreground 'mode-line (cdr color))))))

(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
;(key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "M-c") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "M-c") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "M-c") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "M-c") 'evil-normal-state)
;; (define-key help-mode-map (kbd "i") 'evil-emacs-state)
;; (define-key grep-mode-map (kbd "i") 'evil-emacs-state)

(provide 'init-evil)
