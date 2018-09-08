;;; basic keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key my-keys-minor-mode-map (kbd "C-s") 'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-r") 'isearch-backward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map (bt/kbd "C-w") 'isearch-yank-word-or-char) ;; FIXME 

(define-key my-keys-minor-mode-map (bt/kbd "C-w") 'backward-kill-word)

;; need this so redo can work
(define-key my-keys-minor-mode-map (kbd "C-/") 'undo-tree-undo)
(define-key my-keys-minor-mode-map (kbd "C-y") (lambda () (interactive)
                                                 (yank)
                                                 (call-interactively 'indent-region)))
;; yank without auto-indentation
(define-key my-keys-minor-mode-map (kbd "M-C-y") 'yank)
(define-key my-keys-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)

(global-unset-key (kbd "M-ESC ESC"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x m"))
(define-key my-keys-minor-mode-map (kbd "C-c C-h") '(lambda () (interactive))) ;; global-unset-key wasn't working
(global-set-key (kbd "C-z") nil) ;; can still suspend with C-x C-z

(define-key my-keys-minor-mode-map (bt/kbd "C-,") 'backward-paragraph)
(define-key my-keys-minor-mode-map (bt/kbd "C-.") 'forward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-,") 'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "M-.") 'end-of-buffer)

(define-key my-keys-minor-mode-map (kbd "C-v") 'whole-line-or-region-kill-region)
(define-key my-keys-minor-mode-map (kbd "M-v") 'whole-line-or-region-kill-ring-save)

(define-key my-keys-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key my-keys-minor-mode-map (kbd "ESC M-x") 'execute-extended-command) ;; original M-x

(define-key key-translation-map (kbd "M-; h") [f1])

(define-key my-keys-minor-mode-map (kbd "M-a") 'back-to-indentation)
(define-key my-keys-minor-mode-map (kbd "M-e") 'move-end-of-line)

(define-key my-keys-minor-mode-map (kbd "C-x C-f")   'find-file)
(define-key my-keys-minor-mode-map (kbd "C-x z")     'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-x /")     'winner-undo)
(define-key my-keys-minor-mode-map (kbd "C-x \\")    'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-x -")     'split-window-below)
(define-key my-keys-minor-mode-map (kbd "C-x C-d")   'delete-window)
(define-key my-keys-minor-mode-map (kbd "C-x C-o")   'pop-global-mark)

;; move cursor between windows
(define-key my-keys-minor-mode-map (kbd "C-x h")     'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-x j")     'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-x k")     'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-x l")     'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-x C-h")   'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-x C-j")   'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-x C-k")   'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-x C-l")   'windmove-right)

(define-key my-keys-minor-mode-map (kbd "<backtab>") 'hippie-expand)
(define-key my-keys-minor-mode-map (kbd "M-'")       'whole-line-or-region-comment-dwim)
;; (define-key my-keys-minor-mode-map (kbd "C-x open")     'browse-url-at-point) ;; doesn't work in vagrant

;;  M-; == ;
;; FIXME could have an elisp fn that substitutes all iterm remappings
;; actual FIXME just use ; in these bindings, and bind M-; to insert ";"
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

;; (define-key my-keys-minor-mode-map (kbd "M-; d p") (lambda () (interactive) (mark-paragraph) (kill-region t)))
;; (define-key my-keys-minor-mode-map (kbd "M-; c p") (lambda () (interactive) (mark-paragraph) (whole-line-or-region-comment-dwim "")))

(define-key my-keys-minor-mode-map (kbd "M-; e j") 'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "M-; e i") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "M-; e SPC") 'helm-projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "M-; e RET") (lambda () (interactive) (revert-buffer :ignore-auto :noconfirm)))
(define-key my-keys-minor-mode-map (kbd "M-; e d") 'dired-jump)
(define-key my-keys-minor-mode-map (kbd "M-; e f") 'find-file)

(defun bt/pbcopy ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
  (deactivate-mark))

(defun bt/drag-line-down ()
  (interactive)
  (whole-line-or-region-kill-region 1)
  (move-end-of-line 1)
  (forward-char)
  (yank)
  (previous-line)
  )

(defun bt/drag-line-up ()
  (interactive)
  (whole-line-or-region-kill-region 1)
  (move-beginning-of-line 1)
  (previous-line)
  (yank)
  (previous-line)
  )

(defun bt/join-line-below ()
  "Like 'J' in vim."
  (interactive)
  (next-line)
  (delete-indentation))

(defun bt/forward-to-beginning-of-next-word ()
  "Like 'w' in vim."
  (interactive)
  (if (= 0 (skip-chars-forward "^a-zA-Z"))
      ((lambda ()
         (forward-word)
         (forward-word)
         (backward-word)))))

(define-key my-keys-minor-mode-map (kbd "C-x C-y")   'bt/pbcopy)
(define-key my-keys-minor-mode-map (kbd "M-p") 'bt/drag-line-up)
(define-key my-keys-minor-mode-map (kbd "M-n") 'bt/drag-line-down)
(define-key my-keys-minor-mode-map (kbd "M-j") 'bt/join-line-below)
(define-key my-keys-minor-mode-map (kbd "M-w") 'bt/forward-to-beginning-of-next-word)

;;; buffer name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bt/cp-show (s)
  (shell-command-to-string (concat "echo -n " s "| pbcopy"))
  (message (concat "copied '" s "'"))
  )

(defun bt/buffer-path-from-root()
  (interactive)
  (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal))

(defun bt/cp-show-filepath ()
  (interactive)
  (bt/cp-show (bt/buffer-path-from-root)))

(defun bt/cp-show-full-filepath ()
  (interactive)
  (bt/cp-show (buffer-file-name)))

(defun bt/show-filepath ()
  (interactive)
  (message (bt/buffer-path-from-root)))

(defun bt/show-full-filepath ()
  (interactive)
  (message (buffer-file-name)))

(define-key my-keys-minor-mode-map (kbd "C-x C-p c") 'bt/cp-show-filepath)
(define-key my-keys-minor-mode-map (kbd "C-x C-p C") 'bt/cp-show-full-filepath)
(define-key my-keys-minor-mode-map (kbd "C-x C-p s") 'bt/show-filepath)
(define-key my-keys-minor-mode-map (kbd "C-x C-p S") 'bt/show-full-filepath)

;;; packages (FIXME use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; hide/show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(defun so/set-selective-display-dlw (&optional level)
  "Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (back-to-indentation)
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'so/set-selective-display-dlw)
(define-key my-keys-minor-mode-map (kbd "C-M-u") '(lambda () (interactive) (so/set-selective-display 0)))

;;; ag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-key my-keys-minor-mode-map (kbd "C-x a g s")   'ag-project)
;; (define-key my-keys-minor-mode-map (kbd "C-x a g r")   'ag-project-regexp)

;;; functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doesn't need a keybinding, but useful for macros
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
