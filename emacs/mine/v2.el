(defvar bt/keys-minor-mode-map (make-keymap) "bt/keys-minor-mode keymap.")

(define-minor-mode bt/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " bt" 'bt/keys-minor-mode-map)

(bt/keys-minor-mode 1)

;; make my minor mode the most important minor mode
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'bt/keys-minor-mode))
      (let ((mykeys (assq 'bt/keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'bt/keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(define-key bt/keys-minor-mode-map (kbd "C-h") 'left-char)
(define-key bt/keys-minor-mode-map (kbd "C-j") 'next-line)
(define-key bt/keys-minor-mode-map (kbd "C-k") 'previous-line)
(define-key bt/keys-minor-mode-map (kbd "C-l") 'right-char)



;; prefixes:
;;   r - rectangle
;;   m - set mark in register
;;   j - jump to register (either jump to mark or execute macro)
;;   tfTF - jump to char
;;   k - kill in/around
;;   c - copy in/around
;;   p - paragraph (this could have some overlap with k/c, but use cases are limited so it's ok)

(define-key bt/keys-minor-mode-map (kbd "C-x C-s") (lambda () (interactive) (sleep-for 2) (message "use ;w")))
(define-key bt/keys-minor-mode-map (kbd "M-; q RET") 'delete-window)
(define-key bt/keys-minor-mode-map (kbd "M-; q a RET") 'save-buffers-kill-terminal)
(define-key bt/keys-minor-mode-map (kbd "M-; w")     'save-buffer)
(define-key bt/keys-minor-mode-map (kbd "M-; *")     'isearch-forward-symbol-at-point)

(define-key bt/keys-minor-mode-map (kbd "M-; r i") 'string-insert-rectangle)
(define-key bt/keys-minor-mode-map (kbd "M-; r r") 'string-rectangle)
(define-key bt/keys-minor-mode-map (kbd "M-; r k") 'kill-rectangle)

(define-key bt/keys-minor-mode-map (kbd "M-; k m") 'kmacro-to-register)
(define-key bt/keys-minor-mode-map (kbd "M-; m") 'point-to-register)
(define-key bt/keys-minor-mode-map (kbd "M-; j") 'jump-to-register)

(define-key bt/keys-minor-mode-map (kbd "M-; t") 'vim-til-char)
(define-key bt/keys-minor-mode-map (kbd "M-; f") 'vim-find-char)
(define-key bt/keys-minor-mode-map (kbd "M-; T") 'backwards-vim-til-char)
(define-key bt/keys-minor-mode-map (kbd "M-; F") 'backwards-vim-find-char)

;; FIXME doesn't do anything if you are already on the char that you are killing to
(define-key bt/keys-minor-mode-map (kbd "M-; k t") (lambda (ch) (interactive "c") (kill-with-fn 'vim-til-char ch)))
(define-key bt/keys-minor-mode-map (kbd "M-; k f") (lambda (ch) (interactive "c") (kill-with-fn 'vim-find-char ch)))
(define-key bt/keys-minor-mode-map (kbd "M-; k T") (lambda (ch) (interactive "c") (kill-with-fn 'backwards-vim-til-char ch)))
(define-key bt/keys-minor-mode-map (kbd "M-; k F") (lambda (ch) (interactive "c") (kill-with-fn 'backwards-vim-find-char ch)))

(define-key bt/keys-minor-mode-map (kbd "M-; k a") 'bt/vim-kill-around)
(define-key bt/keys-minor-mode-map (kbd "M-; k i") 'bt/vim-kill-in)
(define-key bt/keys-minor-mode-map (kbd "M-; c a") 'bt/vim-copy-around)
(define-key bt/keys-minor-mode-map (kbd "M-; c i") 'bt/vim-copy-in)

(define-key bt/keys-minor-mode-map (kbd "M-; d p") (lambda () (interactive) (bt/vim-copy-around ?p) (yank-and-indent)))
(define-key bt/keys-minor-mode-map (kbd "M-; c p") (lambda () (interactive) (mark-paragraph) (whole-line-or-region-comment-dwim "")))

(define-key bt/keys-minor-mode-map (kbd "M-; b") 'switch-to-buffer)
(define-key bt/keys-minor-mode-map (kbd "M-; i") 'ibuffer)
(define-key bt/keys-minor-mode-map (kbd "M-; e SPC") (lambda () (interactive) (message " C-j: Find File (keeping session)\nProjectile files") (helm-projectile-find-file)))
(define-key bt/keys-minor-mode-map (kbd "M-; e RET") (lambda () (interactive) (revert-buffer :ignore-auto :noconfirm)))
(define-key bt/keys-minor-mode-map (kbd "M-; s s")     'replace-string)
(define-key bt/keys-minor-mode-map (kbd "M-; s r")     'replace-regexp)
(define-key bt/keys-minor-mode-map (kbd "M-; s q s")   'query-replace)
(define-key bt/keys-minor-mode-map (kbd "M-; s q r")   'query-replace-regexp)

;; (define-key bt/keys-minor-mode-map (kbd "M-; s p v")     'split-window-right)
;; (define-key bt/keys-minor-mode-map (kbd "M-; s p h")     'split-window-below)

(define-key bt/keys-minor-mode-map (kbd "M-; |")     'split-window-right)
(define-key bt/keys-minor-mode-map (kbd "M-; -")     'split-window-below)
