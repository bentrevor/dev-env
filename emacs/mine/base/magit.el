(defalias 'blame 'magit-blame)

(defun bt/git ()
  (interactive)
  (magit-status)
  (delete-other-windows)
  (magit-process-buffer)
  (other-window 1)
  )

(define-key my-keys-minor-mode-map (kbd "M-g M-g") 'bt/git)
