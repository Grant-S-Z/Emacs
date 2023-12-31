;;; pangu-spacing-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pangu-spacing" "pangu-spacing.el" (0 0 0 0))
;;; Generated autoloads from pangu-spacing.el

(autoload 'pangu-spacing-space-current-buffer "pangu-spacing" "\
Space current buffer.
It will really insert separator, no matter what
`pangu-spacing-real-insert-separtor' is." t nil)

(autoload 'pangu-spacing-mode "pangu-spacing" "\
Toggle pangu-spacing-mode

This is a minor mode.  If called interactively, toggle the
`Pangu-Spacing mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pangu-spacing-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-pangu-spacing-mode 'globalized-minor-mode t)

(defvar global-pangu-spacing-mode nil "\
Non-nil if Global Pangu-Spacing mode is enabled.
See the `global-pangu-spacing-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pangu-spacing-mode'.")

(custom-autoload 'global-pangu-spacing-mode "pangu-spacing" nil)

(autoload 'global-pangu-spacing-mode "pangu-spacing" "\
Toggle Pangu-Spacing mode in all buffers.
With prefix ARG, enable Global Pangu-Spacing mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Pangu-Spacing mode is enabled in all buffers where
`pangu-spacing-mode' would do it.

See `pangu-spacing-mode' for more information on Pangu-Spacing mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pangu-spacing" '("pangu-spacing-" "turn-on-pangu-spacing"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pangu-spacing-autoloads.el ends here
