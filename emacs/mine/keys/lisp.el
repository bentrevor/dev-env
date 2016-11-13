(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
(add-hook 'clojure-mode-hook                     #'enable-paredit-mode)
(add-hook 'racket-mode-hook                      #'enable-paredit-mode)
(setq racket-mode-pretty-lambda nil) ;; prevents emacs from showing 'lambda' as 'λ'

(font-lock-add-keywords 'scheme-mode
                        '(("car " . font-lock-builtin-face)
                          ("cdr " . font-lock-builtin-face)
                          ("cons " . font-lock-builtin-face)
                          ))

(put 'if 'scheme-indent-function 3)

(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key my-keys-minor-mode-map (kbd "C-c p u") 'paredit-splice-sexp) ;; unwrap
            (define-key my-keys-minor-mode-map (kbd "C-c p s") 'paredit-split-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p j") 'paredit-join-sexp)

            (define-key my-keys-minor-mode-map (kbd "C-c p i") 'paredit-insert-text)
            (defun paredit-insert-text (txt)
              "ignore paredit rules and just insert text"
              (interactive "stext: ")
              (insert txt))

            (define-key my-keys-minor-mode-map (kbd "C-c p b s") 'paredit-backward-slurp-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p f s") 'paredit-forward-slurp-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p b b") 'paredit-backward-barf-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p f b") 'paredit-forward-barf-sexp)

            (global-paren-face-mode t)
            ))
