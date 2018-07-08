;; aliases
(defalias 'git 'magit-status)
(defalias 'blame 'magit-blame)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")


;; keybindings.el
(define-key my-keys-minor-mode-map (kbd "C-x C-y")   'pbcopy)
(defun pbcopy ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
  (deactivate-mark))

(define-key my-keys-minor-mode-map (kbd "C-x a g s")   'ag-project)
(define-key my-keys-minor-mode-map (kbd "C-x a g r")   'ag-project-regexp)

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

(define-key my-keys-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)

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

;; functions.el
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

(defun pbcopy-filename ()
  (interactive)
  (let ((buffer-name-from-root (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal)))
    (shell-command-to-string (concat "echo -n " buffer-name-from-root "| pbcopy"))))

(define-key my-keys-minor-mode-map (kbd "C-c y f") 'pbcopy-filename)

(defun pbcopy-full-filename ()
  (interactive)
  (shell-command-to-string (concat "echo -n " (buffer-file-name) "| pbcopy")))

(define-key my-keys-minor-mode-map (kbd "C-c y F") 'pbcopy-full-filename)

(define-key my-keys-minor-mode-map (kbd "C-s")       'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-r")       'isearch-backward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-h")       'delete-backward-char)

;; iterm remaps vt102 control codes:
;; C-; to C-\
;; C-' to M-'
;; C-/ to C-_
;; C-, to C-]
;; C-. to C-^
(define-key my-keys-minor-mode-map (kbd "C-]")       'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-^")       'forward-paragraph)

(define-key isearch-mode-map (kbd "C-M-h") 'isearch-yank-word-or-char)

(defun set-selective-display-dlw (&optional level)
  "Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (back-to-indentation)
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(define-key my-keys-minor-mode-map (kbd "C-M-f")       'set-selective-display-dlw)
(define-key my-keys-minor-mode-map (kbd "C-M-u")       '(lambda () (interactive) (set-selective-display 0)))

(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun chomp-end (str)
  "Chomp trailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(defun insert-shell-command (command)
  (interactive "s$ ")
  (insert (chomp-end (concat (shell-command-to-string command))))
  )


;; need this so redo can work
(define-key my-keys-minor-mode-map (kbd "C-/")       'undo-tree-undo)

(define-key my-keys-minor-mode-map (kbd "M-,")       'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "M-.")       'end-of-buffer)

(define-key my-keys-minor-mode-map (kbd "C-M-h")     'backward-kill-word)

(define-key my-keys-minor-mode-map (kbd "C-v")       'whole-line-or-region-kill-region)
(define-key my-keys-minor-mode-map (kbd "M-v")       'whole-line-or-region-kill-ring-save)

(define-key my-keys-minor-mode-map (kbd "M-x")       'helm-M-x)
(define-key my-keys-minor-mode-map (kbd "ESC M-x")   'execute-extended-command) ;; original M-x

(define-key key-translation-map (kbd "M-h") [f1])
(define-key key-translation-map (kbd "M-; h") [f1])

(define-key my-keys-minor-mode-map (kbd "M-a")       'back-to-indentation)
(add-hook 'org-mode-hook (lambda () (local-unset-key (kbd "M-a"))))

(define-key my-keys-minor-mode-map (kbd "M-e")       'move-end-of-line)

;; C-x
(define-key my-keys-minor-mode-map (kbd "C-x f")     'helm-projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x C-f")   'helm-projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x M-f")   'find-file)
(define-key my-keys-minor-mode-map (kbd "C-x z")     'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-x /")     'winner-undo)
(define-key my-keys-minor-mode-map (kbd "C-x \\")    'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-x C-\\")  'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-x -")     'split-window-below)
(define-key my-keys-minor-mode-map (kbd "C-x C-d")   'delete-window)

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
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'hippie-expand)
(define-key my-keys-minor-mode-map (kbd "M-'")       'bt/whole-line-or-region-comment-dwim)
(define-key my-keys-minor-mode-map (kbd "C-x u")     'browse-url-at-point)

(defun bt/whole-line-or-region-comment-dwim ()
  (interactive)

  (if (not mark-active)
      (progn
        (beginning-of-line)
        (set-mark-command nil)
        (end-of-line)
        ))
  (comment-dwim "")
  )


;; (define-key my-keys-minor-mode-map (kbd "ESC <left>") '(lambda () (interactive) (insert "←")))
;; (define-key my-keys-minor-mode-map (kbd "ESC <up>") '(lambda () (interactive) (insert "↑")))
;; (define-key my-keys-minor-mode-map (kbd "ESC <right>") '(lambda () (interactive) (insert "→")))
;; (define-key my-keys-minor-mode-map (kbd "ESC <down>") '(lambda () (interactive) (insert "↓")))



;; yank without auto-indentation
(define-key my-keys-minor-mode-map (kbd "M-C-y") 'yank)

;; yank-and-indent is annoying in haskell
(add-hook 'haskell-mode-hook (lambda () (define-key my-keys-minor-mode-map (kbd "C-y") 'yank)))

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))



(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;; (defun show-ruby-functions ()
;;   (interactive)
;;   (list-matching-lines " def |private") ;; FIXME can't get this function to work from elisp
;; )

;; vim.el
(define-key my-keys-minor-mode-map (kbd "M-j") 'join-line-below)
(defun join-line-below ()
  "Like 'J' in vim."
  (interactive)
  (next-line)
  (delete-indentation))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(smartrep-define-key
    my-keys-minor-mode-map "C-x" '(("C-o" . 'pop-global-mark)
                                   ;; ("C-i" . 'unpop-to-mark-command)
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

;; can still suspend with C-x C-z
(global-set-key (kbd "C-z") nil)

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

(define-key my-keys-minor-mode-map (kbd "M-; q RET") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "M-; q a RET") 'save-buffers-kill-terminal)
(define-key my-keys-minor-mode-map (kbd "M-; w")     'save-buffer)
(define-key my-keys-minor-mode-map (kbd "M-; *")     'isearch-forward-symbol-at-point)

(define-key my-keys-minor-mode-map (kbd "M-; r i") 'string-insert-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-; r r") 'string-rectangle)
(define-key my-keys-minor-mode-map (kbd "M-; r k") 'kill-rectangle)

(define-key my-keys-minor-mode-map (kbd "M-; m") 'point-to-register) ;; mark
(define-key my-keys-minor-mode-map (kbd "M-; j") 'jump-to-register) ;; jump

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

(define-key my-keys-minor-mode-map (kbd "M-; d p") (lambda () (interactive) (bt/vim-copy-around ?p) (yank-and-indent))) ;; dup paragraph
(define-key my-keys-minor-mode-map (kbd "M-; c p") (lambda () (interactive) (mark-paragraph) (bt/whole-line-or-region-comment-dwim))) ;; comment paragraph

(define-key my-keys-minor-mode-map (kbd "M-; e j") 'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "M-; e i") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "M-; e SPC") (lambda () (interactive) (message " C-j: Find File (keeping session)\nProjectile files") (helm-projectile-find-file)))
(define-key my-keys-minor-mode-map (kbd "M-; e RET") (lambda () (interactive) (revert-buffer :ignore-auto :noconfirm)))
(define-key my-keys-minor-mode-map (kbd "M-; e d") 'dired-jump)

(define-key my-keys-minor-mode-map (kbd "M-; s s")     'replace-string)
(define-key my-keys-minor-mode-map (kbd "M-; s r")     'replace-regexp)
(define-key my-keys-minor-mode-map (kbd "M-; s q s")   'query-replace)
(define-key my-keys-minor-mode-map (kbd "M-; s q r")   'query-replace-regexp)

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

(defun bt/smorgeous-api ()
  (interactive)
  (setq projectile-project-root "/Users/ben/code/smorgeous/api")
  )

(defun bt/smorgeous-ui ()
  (interactive)
  (setq projectile-project-root "/Users/ben/code/smorgeous/ui")
  )

(defun bt/fullscreen-git ()

  )
