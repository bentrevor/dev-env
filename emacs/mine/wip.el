
;; vim.el


;;;;;;;;;;;;;;;
;;
;; :<n>{d,p,y,}
;;
(define-key my-keys-minor-mode-map (kbd "M-; M-;")     'open-prompt)

(defmacro on-line-number (line-number cmd)
  `(progn
     (goto-line line-number)
     ,cmd
     (exchange-point-and-mark)))

(defun open-prompt (command)
  "like pressing : in vim."
  (interactive "s:")
  (let ((last-char (substring command -1))
        (line-no (string-to-number (substring command 0 -1))))
    (cond
     ((string-match last-char "dyp") (on-line-number line-no
                                                     (cond
                                                      ((string= last-char "d") (whole-line-or-region-kill-region 1))
                                                      ((string= last-char "y") (whole-line-or-region-kill-ring-save 1))
                                                      ((string= last-char "p") (yank-and-indent)))))
     ((< 0 (string-to-number command)) (goto-line (string-to-number command)))
     (t (message "invalid!")))
    (save-buffer)))


;;;;;;;;;;;;;;;;;;;;
;;
;; {d,v,y}{a,i}{w,p}
;;
(defconst vim-fn-commands '(("d" "delete") ("y" "copy") ("v" "select")))
(defconst vim-fn-scopes '(("i" "in") ("a" "around")))
(defconst vim-fn-boundaries '(("p" "paragraph") ("w" "word") ("b" "parens") ("B" "curly-braces") ("[" "square-brackets")))

(defun vim-til-char (ch)
  (interactive "c")
  (search-forward (char-to-string ch))
  (backward-char)
  )

(defun vim-find-char (ch)
  (interactive "c")
  (search-forward (char-to-string ch))
  )

(defun backwards-vim-til-char (ch)
  (interactive "c")
  (search-backward (char-to-string ch))
  (forward-char)
  )

(defun backwards-vim-find-char (ch)
  (interactive "c")
  (search-backward (char-to-string ch))
  )

(defun kill-with-fn (jump-fn ch)
  (set-mark-command nil)
  (funcall jump-fn ch)
  (kill-region (region-beginning) (region-end))
  )

;; prefixes:
;;   r - rectangle
;;   m - set mark in register
;;   j - jump to register (either jump to mark or execute macro)
;;   tfTF - jump to char
;;   k - kill in/around
;;   c - copy in/around
;;   p - paragraph (this could have some overlap with k/c, but use cases are limited so it's ok)

(define-key my-keys-minor-mode-map (kbd "M-; q RET") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "M-; q a RET") 'save-buffers-kill-terminal)
(define-key my-keys-minor-mode-map (kbd "M-; w")     'save-buffer)
(define-key my-keys-minor-mode-map (kbd "M-; *")     'isearch-forward-symbol-at-point)

(define-key my-keys-minor-mode-map (kbd "M-; r i") 'string-insert-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-; r r") 'string-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-; r k") 'kill-rectangle)

(define-key my-keys-minor-mode-map (kbd "M-; k m") 'kmacro-to-register)
(define-key my-keys-minor-mode-map (kbd "M-; m") 'point-to-register)
(define-key my-keys-minor-mode-map (kbd "M-; j") 'jump-to-register)

(define-key my-keys-minor-mode-map (kbd "M-; t") 'vim-til-char)
(define-key my-keys-minor-mode-map (kbd "M-; f") 'vim-find-char)
(define-key my-keys-minor-mode-map (kbd "M-; T") 'backwards-vim-til-char)
(define-key my-keys-minor-mode-map (kbd "M-; F") 'backwards-vim-find-char)

;; FIXME doesn't do anything if you are already on the char that you are killing to
(define-key my-keys-minor-mode-map (kbd "M-; k t") (lambda (ch) (interactive "c") (kill-with-fn 'vim-til-char ch)))
(define-key my-keys-minor-mode-map (kbd "M-; k f") (lambda (ch) (interactive "c") (kill-with-fn 'vim-find-char ch)))
(define-key my-keys-minor-mode-map (kbd "M-; k T") (lambda (ch) (interactive "c") (kill-with-fn 'backwards-vim-til-char ch)))
(define-key my-keys-minor-mode-map (kbd "M-; k F") (lambda (ch) (interactive "c") (kill-with-fn 'backwards-vim-find-char ch)))

(define-key my-keys-minor-mode-map (kbd "M-; k a") 'bt/vim-kill-around)
(define-key my-keys-minor-mode-map (kbd "M-; k i") 'bt/vim-kill-in)
(define-key my-keys-minor-mode-map (kbd "M-; c a") 'bt/vim-copy-around)
(define-key my-keys-minor-mode-map (kbd "M-; c i") 'bt/vim-copy-in)

(define-key my-keys-minor-mode-map (kbd "M-; d p") (lambda () (interactive) (bt/vim-copy-around ?p) (yank-and-indent)))
(define-key my-keys-minor-mode-map (kbd "M-; c p") (lambda () (interactive) (mark-paragraph) (whole-line-or-region-comment-dwim "")))

(define-key my-keys-minor-mode-map (kbd "M-; e j") 'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "M-; e i") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "M-; e SPC") 'helm-projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "M-; e RET") (lambda () (interactive) (revert-buffer :ignore-auto :noconfirm)))
(define-key my-keys-minor-mode-map (kbd "M-; e d") 'dired-jump)
(define-key my-keys-minor-mode-map (kbd "M-; e f") 'find-file)

(define-key my-keys-minor-mode-map (kbd "M-; d p") (lambda () (interactive) (bt/vim-copy-around ?p) (yank-and-indent)))
(define-key my-keys-minor-mode-map (kbd "M-; c p") (lambda () (interactive) (mark-paragraph) (comment-dwim "")))

(define-key my-keys-minor-mode-map (kbd "M-; s s")     'replace-string)
(define-key my-keys-minor-mode-map (kbd "M-; s r")     'replace-regexp)
(define-key my-keys-minor-mode-map (kbd "M-; s q s")   'query-replace)
(define-key my-keys-minor-mode-map (kbd "M-; s q r")   'query-replace-regexp)

;; (define-key my-keys-minor-mode-map (kbd "M-; s p v")     'split-window-right)
;; (define-key my-keys-minor-mode-map (kbd "M-; s p h")     'split-window-below)

(define-key my-keys-minor-mode-map (kbd "M-; |")     'split-window-right)
(define-key my-keys-minor-mode-map (kbd "M-; -")     'split-window-below)


(defun bt/vim-copy-around (ch)
  (interactive "c")
  (bt/vim-fn "copy" "around" (bt/vim-char-to-boundary ch))
  )

(defun bt/vim-copy-in (ch)
  (interactive "c")
  (bt/vim-fn "copy" "in" (bt/vim-char-to-boundary ch))
  )

(defun bt/vim-kill-around (ch)
  (interactive "c")
  (bt/vim-fn "kill" "around" (bt/vim-char-to-boundary ch))
  )

(defun bt/vim-kill-in (ch)
  (interactive "c")
  (bt/vim-fn "kill" "in" (bt/vim-char-to-boundary ch))
  )

(defun bt/vim-char-to-boundary (ch)
  "b -> (\nB -> {\np -> paragraph\netc."
  (bt/vim-str-to-boundary (char-to-string ch)))

(defun bt/vim-str-to-boundary (ch)
  "b -> (\nB -> {\np -> paragraph\netc."
  (cond
   ((string-match-p (regexp-quote ch) "()b") "(")
   ((string-match-p (regexp-quote ch) "{}B") "{")
   ((string-match-p (regexp-quote ch) "p") "paragraph")
   ((string-match-p (regexp-quote ch) "[]") "[")
   ((string-match-p (regexp-quote ch) "'") "'")
   ((string-match-p (regexp-quote ch) "\"") "\"")
   ))

(defun bt/vim-fn (command boundaries scope)
  (bt/vim-select boundaries scope)

  (cond
   ((string= command "copy") (whole-line-or-region-kill-ring-save 1))
   ((string= command "select") nil) ;; region will always be selected by this point
   ((string= command "kill") (whole-line-or-region-kill-region 1)))

  (message (concat command " " boundaries " " scope)))

(defun bt/vim-select (boundaries scope)
  (cond
   ((string= boundaries "in") (bt/vim-select-in scope))
   ((string= boundaries "around") (bt/vim-select-around scope))))

(defun bt/vim-select-in (scope)
  (cond
   ((string= scope "paragraph") (mark-paragraph) (forward-char))
   (t (bt/vim-select-in-char scope))
   ))

(defun bt/vim-select-around (scope)
  (cond
   ((string= scope "paragraph") (mark-paragraph))
   (t (bt/vim-select-around-char scope))
   ))

(defun bt/vim-select-in-char (char)
  (search-backward char)
  (er/expand-region 1)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char)
  )

(defun bt/vim-select-around-char (char)
  (search-backward char)
  (er/expand-region 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; % (jump to matching paren)
;;
(define-key my-keys-minor-mode-map (kbd "M-; %")     'jump-to-matching-paren)

(defvar open-paren-char ?()
  (defvar closed-paren-char ?))

(defvar open-bracket-char ?{)
(defvar closed-bracket-char ?})

(defvar open-sq-bracket-char ?[)
  (defvar closed-sq-bracket-char ?])

(defun on-opening-paren ()
  (let ((current-char (char-after)))
    (or (char-equal open-paren-char
                    current-char)
        (char-equal open-bracket-char
                    current-char)
        (char-equal open-sq-bracket-char
                    current-char))))

(defun on-closing-paren ()
  (let ((current-char (char-after)))
    (or (char-equal closed-paren-char
                    current-char)
        (char-equal closed-bracket-char
                    current-char)
        (char-equal closed-sq-bracket-char
                    current-char))))

(defun jump-to-opening-paren ()
  (forward-char)
  (er/expand-region 1)
  (keyboard-quit)
  (backward-char))

(defun jump-to-closing-paren ()
  (er/expand-region 1)
  (exchange-point-and-mark)
  (backward-char)
  (keyboard-quit))

(defun jump-to-matching-paren ()
  "Like '%' in vim."
  (interactive)
  (cond
   ((on-opening-paren) (jump-to-closing-paren))
   ((on-closing-paren) (jump-to-opening-paren))
   ))
