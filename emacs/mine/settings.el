(add-to-list 'package-archives       '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'auto-mode-alist        '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(define-key isearch-mode-map [(control h)] 'isearch-delete-char) ;; C-h to delete while searching

;; colors
(load-theme 'yoshi t)
;; (load-theme 'light-soap t)
(set-face-attribute 'fringe nil :background "#FFF")

(setq ag-highlight-search t)

;; line number options
;; (global-linum-mode t)
(setq linum-format "%4d  ")
(setq left-fringe 6)

(add-hook 'ruby-mode-hook #'linum-mode)
(add-hook 'paredit-mode-hook #'linum-mode)
(add-hook 'sh-mode-hook #'linum-mode)
(add-hook 'markdown-mode-hook #'linum-mode)
(add-hook 'js-mode-hook #'linum-mode)
(add-hook 'html-mode-hook #'linum-mode)
(add-hook 'yaml-mode-hook #'linum-mode)
(add-hook 'elm-mode-hook #'linum-mode)


(setq ruby-insert-encoding-magic-comment nil)
(setq x-select-enable-clipboard            t
      x-select-enable-primary              t
      save-interprogram-paste-before-kill  t
      apropos-do-all                       t
      save-place-file (concat user-emacs-directory "places"))

(setq-default auto-save-default nil)                    ;; no autosave
(setq make-backup-files nil)                            ;; no autosave
(setq backup-directory-alist `(("." . "~/.saves")))     ;; save backups in separate directory
(setq-default indent-tabs-mode nil)                     ;; use spaces instead of tabs
(setq-default coffee-tab-width 2)                       ;; coffee mode tab width
(setq-default js-indent-level 2)                        ;; js mode tab width
(setq-default c-basic-offset 8)                         ;; c mode tab width
(show-paren-mode 1)                                     ;; highlight matching parens
(fset 'yes-or-no-p 'y-or-n-p)                           ;; faster prompts
(setq confirm-nonexistent-file-or-buffer nil)           ;; don't prompt to create a new file
(define-key global-map (kbd "RET") 'newline-and-indent) ;; indent after pressing 'enter'
(setq set-mark-command-repeat-pop 1)                    ;; can press C-space to cycle through mark ring (after pressing C-x C-space)
(setq-default scss-compile-at-save nil)                 ;; don't try to compile scss files
(setq-default auto-compression-mode 0)                  ;; don't try to decompress files (like ~/.z)
(setq-default fill-column 100)                          ;; text width for fill-paragraph
(setq-default comment-column 0)                         ;; stop moving comments to the right
(column-number-mode t)                                  ;; show column number
(set-default 'truncate-lines t)                         ;; disable word wrap
;; (setq ruby-deep-indent-paren nil)                       ;; better indentation for multiline hashes in ruby
(winner-mode 1)                                         ;; undo/redo split layout changes
(setq echo-keystrokes 0.001)                            ;; like vim's showcmd
(setq require-final-newline nil)                        ;; don't add newline at end of file

;;;;;;;;;;;;;;;
;;
;; global hooks
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'before-save-hook 'run-rubocop)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; (add-hook 'prog-mode-hook 'highlight-escape-sequences)

;;;;;;;;;;;;;
;;
;; mode hooks
;;
(add-hook 'clojure-mode-hook
          (lambda ()
            (put-clojure-indent 'describe 'defun)
            (put-clojure-indent 'it       'defun)
            (put-clojure-indent 'with     'defun)))

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(add-hook 'erc-mode-hook '(lambda () (setq scroll-conservatively 100)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq gofmt-command "goimports")  ;; use goimports instead of go-fmt
            (add-hook 'before-save-hook 'gofmt-before-save)))

(setq elm-format-on-save t)

(add-hook 'markdown-mode-hook
          (lambda ()
            (remove-hook 'before-save-hook 'delete-trailing-whitespace)))
