(defun python-insert-ipdb ()
  (interactive)
  (indent-new-comment-line)
  (insert "import ipdb; ipdb.set_trace()"))

(defun python-delete-ipdb ()
  (interactive)
  (delete-matching-lines "import ipdb; ipdb.set_trace()"))

(add-hook 'python-mode-hook 
 '(lambda ()
    (define-key python-mode-map (kbd "C-c i") 'python-insert-ipdb)
    (define-key python-mode-map (kbd "C-c d") 'python-delete-ipdb)
   ;; (add-hook 'find-file-hook 'flymake-find-file-hook)
   ;; (when (load "flymake" t)
   ;;  (defun flymake-pyflakes-init ()
   ;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
   ;;  		'flymake-create-temp-inplace))
   ;;  		(local-file
   ;;           (file-relative-name
   ;;  		 temp-file
   ;;  		 (file-name-directory buffer-file-name))))
   ;;  	   (list "pycheckers"  (list local-file))))
   ;;  (add-to-list 'flymake-allowed-file-name-masks
   ;;   '("\\.py\\'" flymake-pyflakes-init)))
   ;; (load-library "flymake-cursor")
   ))

(provide 'init-python)
