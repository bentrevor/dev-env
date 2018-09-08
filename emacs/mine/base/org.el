(autoload 'org "org" "Org mode." t)

(setq bt/org-dir "~/shared/org/")
(defun bt/org-path (x) (concat bt/org-dir x))

(setq org-agenda-files (list (bt/org-path "todo.org")
                             (bt/org-path "projects.org")
                             ))
;; (setq org-agenda-files '())

(setq org-archive-location (bt/org-path "archives/archive.%s::"))

(setq org-startup-indented t)      ;; indent tasks and only show one star
(setq org-catch-invisible-edits t) ;; don't allow edits to collapsed parts of a buffer
(setq org-startup-folded 'content) ;; show all headings at startup

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'before-save-hook 'org-align-all-tags)

                           (define-key my-keys-minor-mode-map (kbd "M-'")   '(lambda () (interactive)))

                           (defun org-journal-entry ()
                             (interactive)
                             (end-of-buffer)
                             (insert "\n\n")
                             (delete-blank-lines)
                             (insert "\n** ")

                             (insert-timestamp-with-time)
                             (insert "\n"))

                           (defun insert-timestamp-with-time ()
                             (interactive)
                             (setq current-prefix-arg '(16))      ; C-u C-u
                             (call-interactively 'org-time-stamp)
                             )

                           (visual-line-mode)    ;; wrap long lines

                           (defun bt/beginning-of-org-line ()
                             (interactive)
                             (beginning-of-line)
                             (search-forward " ")
                             )

                           (define-key my-keys-minor-mode-map (kbd "M-# C-j") 'org-meta-return)
                           (define-key my-keys-minor-mode-map (kbd "C-j") 'org-meta-return)

                           (define-key my-keys-minor-mode-map (kbd "M-# C-a") 'bt/beginning-of-org-line)
                           (define-key my-keys-minor-mode-map (kbd "M-n") 'org-metadown)
                           (define-key my-keys-minor-mode-map (kbd "M-p") 'org-metaup)

                           (defun insert-checkbox-item-on-next-line ()
                             (interactive)
                             (end-of-line)
                             (insert "\n - [ ] ")
                             )

                           (defun insert-checkbox-item ()
                             (interactive)
                             (beginning-of-line)
                             (insert " - [ ] ")
                             )

                           (defun current-line-empty-p ()
                             (save-excursion
                               (beginning-of-line)
                               (looking-at "[[:space:]]*$")))

                           (defun dwim-insert-checkbox-item ()
                             (interactive)
                             (if (current-line-empty-p)
                                 (insert-checkbox-item)
                               (insert-checkbox-item-on-next-line)
                               ))


                           (define-key my-keys-minor-mode-map (kbd "C-j") 'org-meta-return)


                           ;; iterm remaps C-; to M-#
                           (define-key my-keys-minor-mode-map (kbd "M-# b") 'org-backward-heading-same-level)
                           (define-key my-keys-minor-mode-map (kbd "M-# f") 'org-forward-heading-same-level)
                           (define-key my-keys-minor-mode-map (kbd "M-# u") 'outline-up-heading)
                           (define-key my-keys-minor-mode-map (kbd "M-# p") 'outline-previous-visible-heading)
                           (define-key my-keys-minor-mode-map (kbd "M-# n") 'outline-next-visible-heading)

                           (define-key my-keys-minor-mode-map (kbd "M-# C-b") 'org-backward-heading-same-level)
                           (define-key my-keys-minor-mode-map (kbd "M-# C-f") 'org-forward-heading-same-level)
                           (define-key my-keys-minor-mode-map (kbd "M-# C-u") 'outline-up-heading)
                           (define-key my-keys-minor-mode-map (kbd "M-# C-p") 'outline-previous-visible-heading)
                           (define-key my-keys-minor-mode-map (kbd "M-# C-n") 'outline-next-visible-heading)

                           (define-key my-keys-minor-mode-map (kbd "M-# 1") (lambda () (interactive) (org-priority ?A)))
                           (define-key my-keys-minor-mode-map (kbd "M-# 2") (lambda () (interactive) (org-priority ?B)))
                           (define-key my-keys-minor-mode-map (kbd "M-# 3") (lambda () (interactive) (org-priority ?C)))

                           ;; subtree modification
                           (define-key my-keys-minor-mode-map (kbd "M-# B") 'org-promote-subtree)
                           (define-key my-keys-minor-mode-map (kbd "M-# F") 'org-demote-subtree)
                           (define-key my-keys-minor-mode-map (kbd "M-# A") 'org-archive-subtree)

                           (define-key my-keys-minor-mode-map (kbd "M-# C-v") 'org-cut-subtree)
                           (define-key my-keys-minor-mode-map (kbd "M-# C-y") 'org-paste-subtree)

                           (define-key my-keys-minor-mode-map (kbd "M-# C-l") (lambda () (interactive) (switch-to-buffer "*Org Agenda*") (org-todo-list)))
                           (define-key my-keys-minor-mode-map (kbd "M-# l") 'org-todo-list)

                           ; (define-key my-keys-minor-mode-map (kbd "M-# t SPC") (lambda () (interactive) (org-todo "")))
                           ; (define-key my-keys-minor-mode-map (kbd "M-# t t") (lambda () (interactive) (org-todo "TODO")))
                           ; (define-key my-keys-minor-mode-map (kbd "M-# t l") (lambda () (interactive) (org-todo "LIST")))
                           ; (define-key my-keys-minor-mode-map (kbd "M-# t d") (lambda () (interactive) (org-todo "DONE")))
                           ; (define-key my-keys-minor-mode-map (kbd "M-# C-SPC") (lambda () (interactive) (org-todo "")))
                           ; (define-key my-keys-minor-mode-map (kbd "M-# C-t") (lambda () (interactive) (org-todo "TODO")))
                           ; (define-key my-keys-minor-mode-map (kbd "M-# C-l") (lambda () (interactive) (org-todo "LIST")))
                           ; (define-key my-keys-minor-mode-map (kbd "M-# C-d") (lambda () (interactive) (org-todo "DONE")))



                           (smartrep-define-key
                               my-keys-minor-mode-map "M-# M-#" '(
                                                                  ;; basic navigation
                                                                  ("C-b" . 'backward-char)
                                                                  ("C-f" . 'forward-char)
                                                                  ("C-n" . 'next-line)
                                                                  ("C-p" . 'previous-line)
                                                                  ("C-a" . 'org-beginning-of-line)
                                                                  ("C-e" . 'org-end-of-line)

                                                                  ;; subtree navigation
                                                                  ("b" . 'org-backward-heading-same-level)
                                                                  ("f" . 'org-forward-heading-same-level)
                                                                  ("u" . 'outline-up-heading)
                                                                  ("p" . 'outline-previous-visible-heading)
                                                                  ("n" . 'outline-next-visible-heading)
                                                                  ("TAB" . 'org-cycle)
                                                                  ("C-i" . 'org-cycle)

                                                                  ;; subtree modification
                                                                  ("B" . 'org-promote-subtree)
                                                                  ("F" . 'org-demote-subtree)
                                                                  ("A" . 'org-archive-subtree)

                                                                  ("1" . (lambda () (interactive) (org-priority ?A)))
                                                                  ("2" . (lambda () (interactive) (org-priority ?B)))
                                                                  ("3" . (lambda () (interactive) (org-priority ?C)))

                                                                  ("-" . 'org-ctrl-c-minus)
                                                                  ("*" . 'org-ctrl-c-star)

                                                                  ;; these are canceling the smartrep
                                                                  ;; ("M-p" . 'org-metaup)
                                                                  ;; ("M-n" . 'org-metadown)
                                                                  ("t" . (lambda () (interactive) (org-todo "TODO")))
                                                                  ("r" . (lambda () (interactive) (org-todo "REVIEW")))
                                                                  ("m" . (lambda () (interactive) (org-todo "MAYBE")))
                                                                  ("d" . (lambda () (interactive) (org-todo "DONE")))
                                                                  ("c" . (lambda () (interactive) (org-todo "CODE")))
                                                                  ;; not working
                                                                  ;; ("C-M-p" . (lambda () (interactive) (org-todo "PEND")))
                                                                  ;; ("C-M-n" . (lambda () (interactive) (org-todo "NOTE")))
                                                                  ("SPC" . (lambda () (interactive) (org-todo "")))
                                                                  ;; looks like anything with M- will cancel smartrep
                                                                  ;; ("M-v" . 'org-copy-subtree)
                                                                  ("C-v" . 'org-cut-subtree)
                                                                  ("C-y" . 'org-paste-subtree)

                                                                  ))

                           (defun org-date-change-minutes ()
                             (interactive "")
                             (search-forward "]")
                             (backward-char)
                             (backward-char))

                           (defun org-date-change-hours ()
                             (interactive "")
                             (search-forward "]")
                             (backward-char)
                             (backward-char)
                             (backward-char)
                             (backward-char)
                             (backward-char))

                           (smartrep-define-key
                               my-keys-minor-mode-map "M-# d" '(("b" . 'org-timestamp-down-day)
                                                                ("f" . 'org-timestamp-up-day)
                                                                ("p" . 'org-timestamp-up)
                                                                ("n" . 'org-timestamp-down)

                                                                ("h" . 'org-date-change-hours)
                                                                ("m" . 'org-date-change-minutes)
                                                                ("c" . 'org-timestamp-down)

                                                                ("C-f" . 'forward-char)
                                                                ("C-b" . 'backward-char)
                                                                ("C-n" . 'next-line)
                                                                ("C-p" . 'previous-line)
                                                                ))

                           (define-key helm-map (kbd "M-p") 'previous-history-element)
                           (define-key helm-map (kbd "M-n") 'next-history-element)


                           ))
