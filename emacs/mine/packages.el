(autoload 'zap-up-to-char "misc" "like 't' in vim" 'interactive)
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)

(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rubbishfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(setq web-mode-markup-indent-offset 2) ;; html indent
(setq web-mode-code-indent-offset 2) ;; js indent

(add-to-list 'auto-mode-alist '("zshenv" . shell-script-mode))
(add-to-list 'auto-mode-alist '("zshrc" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(require 'projectile)

(projectile-global-mode) ;; like command-t

(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)
(helm-projectile-on)

(setq projectile-globally-ignored-directories '(
                                                "node_modules"
                                                "app/assets"
                                                "tmp"
                                                "vendor"
                                                "elpa"
                                                ))

;; So that helm does not use current window to display the helm window
(setq helm-split-window-in-side-p t)

;; make helm always open at the bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(font-lock-add-keywords 'javascript-mode
                        '(("Math" . font-lock-type-face)
                          ("Number" . font-lock-type-face)
                          ("Date" . font-lock-type-face)
                          ("String" . font-lock-type-face)
                          ("RegExp" . font-lock-type-face)
                          ("Array" . font-lock-type-face)
                          ("JSON" . font-lock-type-face)
                          ("Object" . font-lock-type-face)
                          ))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'expand-region)
(global-set-key (kbd "M-s i") 'er/expand-region)
(global-set-key (kbd "M-s M-i") 'er/expand-region)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'whole-line-or-region)
(whole-line-or-region-mode)

;; for easy keybindings with single-key repeats
(require 'smartrep)
