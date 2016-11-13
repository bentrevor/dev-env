;; define my very own minor mode
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

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

;; unset
(global-unset-key (kbd "M-ESC ESC"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x m"))
(define-key my-keys-minor-mode-map (kbd "C-c C-h")   '(lambda () (interactive))) ;; global-unset-key wasn't working

(define-key my-keys-minor-mode-map (kbd "C-s")       'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-r")       'isearch-backward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-h")       'delete-backward-char)

;; karabiner remaps vt102 control codes:
;; C-' to C-\
;; C-/ to C-_
;; C-, to C-]
;; C-. to C-^
(define-key my-keys-minor-mode-map (kbd "C-]")       'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-^")       'forward-paragraph)

(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(defun insert-shell-command (command)
  (interactive "s$ ")
  (insert (chomp-end (concat (shell-command-to-string command))))
  )

(define-key my-keys-minor-mode-map (kbd "C-\\")      'insert-shell-command)

;; need this so redo can work
(define-key my-keys-minor-mode-map (kbd "C-/")       'undo-tree-undo)

(define-key my-keys-minor-mode-map (kbd "M-,")       'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "M-.")       'end-of-buffer)

(define-key my-keys-minor-mode-map (kbd "C-w")       'backward-kill-word)
(define-key helm-map               (kbd "C-w")       'backward-kill-word)

(define-key my-keys-minor-mode-map (kbd "C-v")       'whole-line-or-region-kill-region)
(define-key my-keys-minor-mode-map (kbd "M-v")       'whole-line-or-region-kill-ring-save)

(define-key my-keys-minor-mode-map (kbd "M-x")       'helm-M-x)
(define-key my-keys-minor-mode-map (kbd "ESC M-x")   'execute-extended-command) ;; original M-x

;; C-x
(define-key my-keys-minor-mode-map (kbd "C-x d k")   'describe-key)
(define-key my-keys-minor-mode-map (kbd "C-x f")     'helm-projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x C-f")   'helm-projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x M-f")   'find-file)
(define-key my-keys-minor-mode-map (kbd "C-x z")     'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-x /")     'winner-undo)
(define-key my-keys-minor-mode-map (kbd "C-x \\")    'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-x C-\\")  'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-x -")     'split-window-below)
(define-key my-keys-minor-mode-map (kbd "C-x C-d")   'delete-window)

;; M-r  -->  rectangle
(define-key my-keys-minor-mode-map (kbd "M-r r")     'string-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-r i")     'string-insert-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-r k")     'kill-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-r y")     'yank-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-r M-v")   'copy-rectangle-as-kill)

(define-key my-keys-minor-mode-map (kbd "C-x C-b l")     'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x C-b L")     'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-x C-b r")     'revert-buffer)

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(define-key my-keys-minor-mode-map (kbd "C-x r s")     'replace-string)
(define-key my-keys-minor-mode-map (kbd "C-x r b")     'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x q r s")   'query-replace-string)

;; move cursor between windows
(define-key my-keys-minor-mode-map (kbd "C-x h")     'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-x j")     'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-x k")     'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-x l")     'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-x C-h")   'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-x C-j")   'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-x C-k")   'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-x C-l")   'windmove-right)

;; other
(define-key my-keys-minor-mode-map (kbd "C-x g i t")     'magit-status)
(define-key my-keys-minor-mode-map (kbd "C-x g h")     'open-in-github)
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'hippie-expand)
(define-key my-keys-minor-mode-map (kbd "M-;")       'whole-line-or-region-comment-dwim)
(define-key my-keys-minor-mode-map (kbd "C-x u")     'browse-url-at-point)

;; s == scroll
(smartrep-define-key
    my-keys-minor-mode-map "C-c s" '(("n" . 'scroll-up-line)
                                     ("p" . 'scroll-down-line)
                                     ))

;; yank without auto-indentation
(define-key my-keys-minor-mode-map (kbd "M-C-y") 'yank)

;; yank-and-indent is annoying in haskell
(add-hook 'haskell-mode-hook (lambda () (define-key my-keys-minor-mode-map (kbd "C-y") 'yank)))
(add-hook 'haskell-mode-hook (lambda () (define-key my-keys-minor-mode-map (kbd "<backtab>") 'hippie-expand)))


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
