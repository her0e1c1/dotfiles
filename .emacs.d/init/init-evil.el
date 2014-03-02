;; -*- lexical-binding: t -*-
(require 'evil)
(evil-mode 1)
; evilのstateは下記の通り
; insert-mode, normal-mode, visual-mode

(defun toggle-evil-mode()
  (interactive)
  (evil-mode 'toggle))

;; stateによって色を変更する
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
;; (key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
;; (define-key evil-insert-state-map (kbd "M-c") 'evil-normal-state)
;; (define-key evil-replace-state-map (kbd "M-c") 'evil-normal-state)
;; (define-key evil-normal-state-map (kbd "M-c") 'evil-normal-state)
;; (define-key evil-visual-state-map (kbd "M-c") 'evil-normal-state)
;; (define-key help-mode-map (kbd "i") 'evil-emacs-state)
;; (define-key grep-mode-map (kbd "i") 'evil-emacs-state)

;;; C-c as general purpose escape key sequence.
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
;; Works around the fact that Evil uses read-event directly when in operator state, which
;; doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
;; Not sure what behavior this changes, but might as well set it, seeing the Elisp manual's
;; documentation of it.
(set-quit-char "C-c")

; prefixをCtrlではなくcに変更
(define-key evil-normal-state-map "c" nil)
(define-key evil-motion-state-map "cu" 'universal-argument)
(define-key key-translation-map (kbd "ch") (kbd "C-h"))
(define-key key-translation-map (kbd "cx") (kbd "C-x"))
(define-key key-translation-map (kbd "cc") (kbd "C-c"))

;; Nomal Mode
; SPCでカーソルを好きな位置に移動させる
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)  

(provide 'init-evil)
