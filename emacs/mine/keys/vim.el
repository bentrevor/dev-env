
(define-key my-keys-minor-mode-map (kbd "M-j") 'join-line-below)
(defun join-line-below ()
  "Like 'J' in vim."
  (interactive)
  (next-line)
  (delete-indentation))

(define-key my-keys-minor-mode-map (kbd "C-x C-o")   'switch-to-previous-buffer)

(smartrep-define-key
    my-keys-minor-mode-map "C-x" '(("C-o" . 'switch-to-previous-buffer)
                                   ))
(defun switch-to-previous-buffer ()
  "Like 'C-o' in vim (but it can only go back one buffer...)."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(define-key my-keys-minor-mode-map (kbd "M-w")   'forward-to-beginning-of-next-word)
(defun forward-to-beginning-of-next-word ()
  "Like 'w' in vim."
  (interactive)
  (if (= 0 (skip-chars-forward "^a-zA-Z"))
      ((lambda ()
         (forward-word)
         (forward-word)
         (backward-word)))))

;;;;;;;;;;;;;;;
;;
;; :<n>{d,p,y,}
;;
(define-key my-keys-minor-mode-map (kbd "C-c ;")     'open-prompt)
(define-key my-keys-minor-mode-map (kbd "C-c C-\\")  'open-prompt)

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

(define-key my-keys-minor-mode-map (kbd "C-c d a p") 'delete-around-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c d a w") 'delete-around-word)
(define-key my-keys-minor-mode-map (kbd "C-c d a b") 'delete-around-parens)
(define-key my-keys-minor-mode-map (kbd "C-c d a B") 'delete-around-curly-braces)
(define-key my-keys-minor-mode-map (kbd "C-c d i p") 'delete-in-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c d i w") 'delete-in-word)
(define-key my-keys-minor-mode-map (kbd "C-c d i b") 'delete-in-parens)
(define-key my-keys-minor-mode-map (kbd "C-c d i B") 'delete-in-curly-braces)

(define-key my-keys-minor-mode-map (kbd "C-c y a p") 'copy-around-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c y a w") 'copy-around-word)
(define-key my-keys-minor-mode-map (kbd "C-c y a b") 'copy-around-parens)
(define-key my-keys-minor-mode-map (kbd "C-c y a B") 'copy-around-curly-braces)
(define-key my-keys-minor-mode-map (kbd "C-c y i p") 'copy-in-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c y i w") 'copy-in-word)
(define-key my-keys-minor-mode-map (kbd "C-c y i b") 'copy-in-parens)
(define-key my-keys-minor-mode-map (kbd "C-c y i B") 'copy-in-curly-braces)

(defun pbcopy-filename ()
  (interactive)
  (shell-command-to-string (concat "echo -n " (buffer-file-name) "| pbcopy")))

(define-key my-keys-minor-mode-map (kbd "C-c y f") 'pbcopy-filename)

(defun pbcopy-rspec-it ()
  (interactive)
  (shell-command-to-string (concat "echo -n " (progn (re-search-backward "^ *it ")
                                                     (forward-to-beginning-of-next-word)
                                                     (forward-to-beginning-of-next-word)
                                                     (backward-char)
                                                     (set-mark-command nil)
                                                     (end-of-line)
                                                     (backward-char) (backward-char) (backward-char)
                                                     (kill-ring-save (region-beginning) (region-end))
                                                     (yank)) "| pbcopy")))

(define-key my-keys-minor-mode-map (kbd "C-c v a p") 'select-around-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c v a w") 'select-around-word)
(define-key my-keys-minor-mode-map (kbd "C-c v a b") 'select-around-parens)
(define-key my-keys-minor-mode-map (kbd "C-c v a B") 'select-around-curly-braces)
(define-key my-keys-minor-mode-map (kbd "C-c v i p") 'select-in-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c v i w") 'select-in-word)
(define-key my-keys-minor-mode-map (kbd "C-c v i b") 'select-in-parens)
(define-key my-keys-minor-mode-map (kbd "C-c v i B") 'select-in-curly-braces)

;; each command
;;   each scope
;;     each boundary
;;       (define-key my-keys-minor-mode-map (kbd (concat "C-c " command scope boundary)) '(TODO))

(defun delete-around-paragraph ()    (interactive) (vim-function "delete" "around" "paragraph"))
(defun delete-around-word ()         (interactive) (vim-function "delete" "around" "word"))
(defun delete-around-parens ()       (interactive) (vim-function "delete" "around" "parens"))
(defun delete-around-curly-braces () (interactive) (vim-function "delete" "around" "curly-braces"))
(defun delete-in-paragraph ()        (interactive) (vim-function "delete" "in" "paragraph"))
(defun delete-in-word ()             (interactive) (vim-function "delete" "in" "word"))
(defun delete-in-parens ()           (interactive) (vim-function "delete" "in" "parens"))
(defun delete-in-curly-braces ()     (interactive) (vim-function "delete" "in" "curly-braces"))

(defun copy-around-paragraph ()      (interactive) (vim-function "copy" "around" "paragraph"))
(defun copy-around-word ()           (interactive) (vim-function "copy" "around" "word"))
(defun copy-around-parens ()         (interactive) (vim-function "copy" "around" "parens"))
(defun copy-around-curly-braces ()   (interactive) (vim-function "copy" "around" "curly-braces"))
(defun copy-in-paragraph ()          (interactive) (vim-function "copy" "in" "paragraph"))
(defun copy-in-word ()               (interactive) (vim-function "copy" "in" "word"))
(defun copy-in-parens ()             (interactive) (vim-function "copy" "in" "parens"))
(defun copy-in-curly-braces ()       (interactive) (vim-function "copy" "in" "curly-braces"))

(defun select-around-paragraph ()    (interactive) (vim-function "select" "around" "paragraph"))
(defun select-around-word ()         (interactive) (vim-function "select" "around" "word"))
(defun select-around-parens ()       (interactive) (vim-function "select" "around" "parens"))
(defun select-around-curly-braces () (interactive) (vim-function "select" "around" "curly-braces"))
(defun select-in-paragraph ()        (interactive) (vim-function "select" "in" "paragraph"))
(defun select-in-word ()             (interactive) (vim-function "select" "in" "word"))
(defun select-in-parens ()           (interactive) (vim-function "select" "in" "parens"))
(defun select-in-curly-braces ()     (interactive) (vim-function "select" "in" "curly-braces"))

(defun vim-function (command boundaries scope)
  (vim-select boundaries scope)

  (cond
   ((string= command "copy") (whole-line-or-region-kill-ring-save 1))
   ((string= command "select") nil) ;; region will always be selected by this point
   ((string= command "delete") (whole-line-or-region-kill-region 1)))

  (message (concat command " " boundaries " " scope)))

(defun vim-select (boundaries scope)
  (cond
   ((string= boundaries "in") (vim-select-in scope))
   ((string= boundaries "around") (vim-select-around scope))))

(defun vim-select-in (scope)
  (cond
   ((string= scope "word") (er/expand-region 1))
   ((string= scope "paragraph") (mark-paragraph) (forward-char))
   ((string= scope "parens") (select-in "("))
   ((string= scope "curly-braces") (select-in "{"))
   ((string= scope "square-brackets") (select-in "["))
   ))

(defun vim-select-around (scope)
  (cond
   ((string= scope "word") (er/expand-region 1)) ;; TODO this works the same as "in" right now (but I never used `daw` much anyway)
   ((string= scope "paragraph") (mark-paragraph))
   ((string= scope "parens") (select-around "("))
   ((string= scope "curly-braces") (select-around "{"))
   ((string= scope "square-brackets") (select-around "["))
   ))

(defun select-in (char)
  (search-backward char)
  (er/expand-region 1)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char)
  )

(defun select-around (char)
  (search-backward char)
  (er/expand-region 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; % (jump to matching paren)
;;
(define-key my-keys-minor-mode-map (kbd "C-c %")     'jump-to-matching-paren)

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
  (if (on-opening-paren)
      (jump-to-closing-paren)
    (jump-to-opening-paren)))
