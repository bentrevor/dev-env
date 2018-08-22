(autoload 'org "org" "Org mode." t)

(setq org-agenda-files '("~/org/todo.org"))

(setq org-archive-location "~/org/old.org_archive")
(setq org-startup-indented t)             ;; indent tasks and only show one star
(setq org-log-done nil)                   ;; no timestamp when task moves to DONE
(setq org-enforce-todo-dependencies t)    ;; can't finish a task when a subtask is incomplete
;; (setq org-agenda-todo-list-sublevels nil) ;; only show top-level TODO tasks in agenda todo list
(setq org-clock-clocktable-default-properties '(:maxlevel 4))
(setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)) ;; format time to not show days in clock tables


;; (setq org-tags-exclude-from-inheritance '("todo"))
(setq org-todo-keywords
      '((type "TODO" "LIST" "|" "SKIP" "DONE")))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

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

                           (defun beginning-of-org-line ()
                             (interactive)
                             (beginning-of-line)
                             (search-forward " ")
                             )

                           (defun all-the-way-down ()
                             (interactive)
                             (dotimes (n 10 r)
                               (org-metadown)))

                           (defun all-the-way-up ()
                             (interactive)
                             (dotimes (n 10 r)
                               (org-metaup)))

                           (define-key my-keys-minor-mode-map (kbd "M-# C-j") 'org-meta-return)
                           (define-key my-keys-minor-mode-map (kbd "C-j") 'org-meta-return)

                           (define-key my-keys-minor-mode-map (kbd "M-a") 'beginning-of-org-line)
                           (define-key my-keys-minor-mode-map (kbd "M-n") 'org-metadown)
                           (define-key my-keys-minor-mode-map (kbd "M-p") 'org-metaup)
                           (define-key my-keys-minor-mode-map (kbd "ESC M-n") 'all-the-way-down)
                           (define-key my-keys-minor-mode-map (kbd "ESC M-p") 'all-the-way-up)

                           (defun bt/org-archive-next-done ()
                             (interactive)
                             (re-search-forward "^\** DONE ")
                             (org-archive-subtree)
                             )

                           (define-key my-keys-minor-mode-map (kbd "C-x C-x") 'bt/org-archive-all-done)

                           (defun bt/org-archive-all-done ()
                             (interactive)
                             (dotimes (n 100 r)
                               (bt/org-archive-next-done)))




                           ;; these are basically the same as org commands that can be run with C-c
                           ;; (define-key my-keys-minor-mode-map (kbd "M-# /") 'org-sparse-tree)

                           ;; TODO should defadvice this
                           (defun todo-checkbox ()
                             (interactive)
                             (org-todo "TODO")
                             (beginning-of-line)
                             (forward-word)
                             (insert " [/]")
                             (backward-char)
                             (org-ctrl-c-ctrl-c)
                             )

                           ;; (define-key my-keys-minor-mode-map (kbd "C-\\ /") 'todo-checkbox)

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


                           ;; (define-key my-keys-minor-mode-map (kbd "C-\\ [") 'dwim-insert-checkbox-item)

                           ;; org-tree-to-indirect-buffer is pretty close to this, but it gets
                           ;; messed up sometimes and I'm not sure why, so manually copying it back
                           ;; over seems like the best way to do this
                           (defun org-subtree-in-new-file ()
                             (interactive)
                             (let ((timestamp (concat (number-to-string (cadr (current-time)))
                                                      (number-to-string (caddr (current-time)))
                                                      )))
                               (org-copy-subtree)
                               (find-file (concat "tmp-" timestamp ".org"))
                               (yank)
                               (delete-other-windows)
                               )
                             )



                           ;; iterm remaps C-; to M-#
                           (define-key my-keys-minor-mode-map (kbd "M-# c i") 'org-clock-in)
                           (define-key my-keys-minor-mode-map (kbd "M-# c o") 'org-clock-out)
                           (define-key my-keys-minor-mode-map (kbd "M-# c g") 'org-clock-goto)
                           (define-key my-keys-minor-mode-map (kbd "M-# [") 'dwim-insert-checkbox-item)

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

                           (define-key my-keys-minor-mode-map (kbd "M-# !") 'org-subtree-in-new-file)

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
