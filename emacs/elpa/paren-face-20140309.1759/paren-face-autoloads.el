;;; paren-face-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-paren-face-mode paren-face-mode) "paren-face"
;;;;;;  "paren-face.el" (21544 5607 0 0))
;;; Generated autoloads from paren-face.el

(autoload 'paren-face-mode "paren-face" "\
Use a dedicated face just for parentheses.

\(fn &optional ARG)" t nil)

(defvar global-paren-face-mode nil "\
Non-nil if Global-Paren-Face mode is enabled.
See the command `global-paren-face-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-paren-face-mode'.")

(custom-autoload 'global-paren-face-mode "paren-face" nil)

(autoload 'global-paren-face-mode "paren-face" "\
Toggle Paren-Face mode in all buffers.
With prefix ARG, enable Global-Paren-Face mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Paren-Face mode is enabled in all buffers where
`turn-on-paren-face-mode-if-desired' would do it.
See `paren-face-mode' for more information on Paren-Face mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("paren-face-pkg.el") (21544 5607 601996
;;;;;;  0))

;;;***

(provide 'paren-face-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; paren-face-autoloads.el ends here
