;;; Local Variables:
;;; eval: (rainbow-mode)
;;; End:

(deftheme btcolor
  "simple theme that uses terminal's colors, mostly copied from badger-theme.el")

;; Not a bad idea to define a palette...

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

    ("darkgray" . "color-235")
    ("bggray" . "color-236")
    ("gray" . "color-238")
    ("lightgray" . "color-240")

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
  `(linum ((t (:foreground ,gray :background ,bggray))))
  `(region ((t (:background ,darkgray))))

  ;; match at cursor
  `(isearch ((t (:background ,brightyellow :foreground ,black))))
  ;; other matches
  `(lazy-highlight ((t (:background ,green :foreground ,black :weight bold))))

  `(success ((t (:foreground ,green :weight bold))))
  `(warning ((t (:foreground ,red :weight bold))))

  `(mode-line ((t (:background ,blue :foreground ,black ))))
  `(mode-line-inactive ((t (:background ,gray :foreground ,black ))))
  `(mode-line-buffer-id ((t (:foreground ,brightyellow :weight bold))))
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

  `(magit-diff-removed ((t (:background ,red :foreground ,black))))
  `(magit-diff-added ((t (:background ,green :foreground ,black))))

  `(magit-diff-removed-highlight ((t (:background ,brightred :foreground ,black))))
  `(magit-diff-added-highlight ((t (:background ,brightgreen :foreground ,black))))

  ))

(provide-theme 'btcolor)
