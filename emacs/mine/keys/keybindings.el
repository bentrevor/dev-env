;; define my very own minor mode
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(load "~/.emacs.d/mine/keys/aliases.el")
(load "~/.emacs.d/mine/keys/builtins.el")
(load "~/.emacs.d/mine/keys/erc.el")
(load "~/.emacs.d/mine/keys/lisp.el")
(load "~/.emacs.d/mine/keys/vim.el")

(define-key my-keys-minor-mode-map (kbd "C-x C-y")   'pbcopy)
(defun pbcopy ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
  (deactivate-mark))

(define-key my-keys-minor-mode-map (kbd "C-x a g s")   'ag-project)
(define-key my-keys-minor-mode-map (kbd "C-x a g r")   'ag-project-regexp)

(define-key my-keys-minor-mode-map (kbd "C-c k")     'zap-up-to-char)
(define-key my-keys-minor-mode-map (kbd "C-c K")     'kill-to-string)
(defun kill-to-string (target)
  (interactive "skill to string: ")
  (set-mark-command nil)
  (search-forward target)
  (search-backward target)
  (kill-region (region-beginning) (region-end)))

(define-key my-keys-minor-mode-map (kbd "C-y") 'yank-and-indent)
(defun yank-and-indent ()
  "Yank with correct indentation."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

;; n == number
(smartrep-define-key
    my-keys-minor-mode-map "C-c n" '(("p" . 'increment-next-number)
                                     ("n" . 'decrement-next-number)
                                     ))
(defun increment-next-number (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-next-number (&optional arg)
  (interactive "p*")
  (increment-next-number (if arg (- arg) -1)))

;; alias notes='emacs -e "just-text"'
(defun just-text ()
  (setq mode-line-format nil)
  (global-linum-mode 0)
  (define-key my-keys-minor-mode-map (kbd "C-x C-s") (lambda () (interactive) (message "can't save in notes mode")))
  (setq initial-scratch-message "")
  )

;; mode ;;
;;;;;;;;;;
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; make my minor mode the most important minor mode
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)
