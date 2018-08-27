;;; Local Variables:
;;; eval: (rainbow-mode)
;;; End:

(deftheme btcolor
  "simple theme that uses terminal's colors, mostly copied from badger-theme.el")

(defvar btcolor/default-colors-alist
  '(
    ("white" . "white")
    ("brightwhite" . "brightwhite")
    ("black" . "black")
    ("brightblack" . "brightblack")

    ("red" . "red")
    ("brightred" . "brightred")

    ("green" . "green")
    ("brightgreen" . "brightgreen")

    ("blue" . "blue")
    ("brightblue" . "brightblue")

    ("yellow" . "yellow")
    ("brightyellow" . "brightyellow")

    ("cyan" . "cyan")
    ("brightcyan" . "brightcyan")

    ("magenta" . "magenta")
    ("brightmagenta" . "brightmagenta")

    ;; normal colors
    ;; ("darkgray" . "color-235")
    ;; ("gray" . "color-238")
    ;; ("lightgray" . "color-240")

    ;; inverted colors
    ("darkgray" . "color-252")
    ("gray" . "color-246")
    ("lightgray" . "color-242")

    ))

(
 defmacro btcolor/with-color-variables (&rest body)
  "`let' bind all colors defined in `btcolor/colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
		   btcolor/default-colors-alist))
     ,@body))

(btcolor/with-color-variables
 (custom-theme-set-faces
  'btcolor

  `(default ((t (:foreground ,white :background ,black))))
  `(linum ((t (:foreground ,gray :background ,brightblack))))
  `(region ((t (:background ,darkgray))))

  ;; match at cursor
  `(isearch ((t (:background ,brightyellow :foreground ,black))))
  ;; other matches
  `(lazy-highlight ((t (:background ,green :foreground ,black :weight bold))))

  `(success ((t (:foreground ,green :weight bold))))
  `(warning ((t (:foreground ,red :weight bold))))

  `(mode-line ((t (:background ,green :foreground ,black ))))
  `(mode-line-inactive ((t (:background ,gray :foreground ,black ))))
  `(mode-line-buffer-id ((t (:foreground ,brightblack :weight bold))))
  `(minibuffer-prompt ((t (:foreground ,blue))))

  `(font-lock-builtin-face ((t (:foreground ,brightblue))))
  `(font-lock-comment-delimiter-face ((t (:foreground ,gray))))
  `(font-lock-comment-face ((t (:foreground ,gray))))
  `(font-lock-constant-face ((t (:foreground ,yellow))))
  `(font-lock-function-name-face ((t (:foreground ,red))))
  `(font-lock-keyword-face ((t (:foreground ,blue))))
  `(font-lock-negation-char-face ((t (:foreground ,red))))
  `(font-lock-string-face ((t (:foreground ,yellow))))
  `(font-lock-type-face ((t (:foreground ,green))))
  `(font-lock-variable-name-face ((t (:foreground ,magenta))))

  `(helm-selection ((t (:background ,gray :weight bold))))
  `(helm-source-header ((t (:background ,red :weight bold))))

  `(diff-removed ((t (:background ,red :foreground ,black))))
  `(diff-added ((t (:background ,green :foreground ,black))))

  `(diff-refine-removed ((t (:background ,brightred :foreground ,black))))
  `(diff-refine-added ((t (:background ,brightgreen :foreground ,black))))


;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,darkgray))))
   `(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,red :weight bold))))

   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))

   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,darkgray :weight bold))))
   ;; `(magit-diff-file-heading-selection ((t (:background ,green :foreground ,cyan :weight bold))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,cyan :foreground ,black))))
   `(magit-diff-hunk-heading           ((t (:background ,darkgray :foreground ,cyan))))
   `(magit-diff-hunk-heading-selection ((t (:background ,brightmagenta :foreground ,cyan))))
   `(magit-diff-lines-heading          ((t (:background ,cyan :foreground ,gray))))
   `(magit-diff-context-highlight      ((t (:background ,darkgray))))

   `(magit-diffstat-added   ((t (:foreground ,green))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,brightgreen :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,darkgray    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,blue  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,cyan))))
   `(magit-log-date      ((t (:foreground ,gray))))
   `(magit-log-graph     ((t (:foreground ,gray))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,yellow))))
   `(magit-sequence-stop ((t (:foreground ,green))))
   `(magit-sequence-part ((t (:foreground ,brightyellow))))
   `(magit-sequence-head ((t (:foreground ,blue))))
   `(magit-sequence-drop ((t (:foreground ,red))))
   `(magit-sequence-done ((t (:foreground ,gray))))
   `(magit-sequence-onto ((t (:foreground ,gray))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,green))))
   `(magit-bisect-skip ((t (:foreground ,yellow))))
   `(magit-bisect-bad  ((t (:foreground ,red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,gray :foreground ,blue))))
   `(magit-blame-hash    ((t (:background ,gray :foreground ,blue))))
   `(magit-blame-name    ((t (:background ,gray :foreground ,cyan))))
   `(magit-blame-date    ((t (:background ,gray :foreground ,cyan))))
   `(magit-blame-summary ((t (:background ,gray :foreground ,blue :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,gray))))
   `(magit-hash           ((t (:foreground ,gray))))
   `(magit-tag            ((t (:foreground ,cyan :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,blue   :weight bold))))
   `(magit-refname        ((t (:background ,lightgray :foreground ,white :weight bold))))
   `(magit-refname-stash  ((t (:background ,lightgray :foreground ,white :weight bold))))
   `(magit-refname-wip    ((t (:background ,lightgray :foreground ,white :weight bold))))
   `(magit-signature-good      ((t (:foreground ,green))))
   `(magit-signature-bad       ((t (:foreground ,red))))
   `(magit-signature-untrusted ((t (:foreground ,yellow))))
   `(magit-signature-expired   ((t (:foreground ,cyan))))
   `(magit-signature-revoked   ((t (:foreground ,magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,magenta))))
   `(magit-reflog-commit       ((t (:foreground ,green))))
   `(magit-reflog-amend        ((t (:foreground ,magenta))))
   `(magit-reflog-merge        ((t (:foreground ,green))))
   `(magit-reflog-checkout     ((t (:foreground ,blue))))
   `(magit-reflog-reset        ((t (:foreground ,red))))
   `(magit-reflog-rebase       ((t (:foreground ,magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,green))))
   `(magit-reflog-remote       ((t (:foreground ,cyan))))
   `(magit-reflog-other ((t (:foreground ,cyan))))

  `(outline-1 ((t (:foreground ,red))))
  `(outline-2 ((t (:foreground ,green))))
  `(outline-3 ((t (:foreground ,magenta))))
  `(outline-4 ((t (:foreground ,blue))))
  `(outline-5 ((t (:foreground ,brightred))))
  `(outline-6 ((t (:foreground ,brightgreen))))
  `(outline-7 ((t (:foreground ,brightmagenta))))
  `(outline-8 ((t (:foreground ,brightblue))))


  ))

(provide-theme 'btcolor)
