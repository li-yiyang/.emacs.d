;;; init-tui.el --- Init for TUI.

;; use tao-yang as theme, prevent blink-search error
;; see https://github.com/manateelazycat/blink-search/issues/16

(require 'tao-theme)
(require 'tao-yang-theme)

(defcustom ryo.ui:tui-theme 'tao-yang
  "The default TUI theme. "
  :group 'ryo.ui)

(defcustom ryo.ui:tui-color-value-fallback
  (color-values "#FFFFFF")
  "The color value fallback for `color-values'.
see \\=`ryo.ui:tui-patch-color-values\\='. "
  :group 'ryo.ui
  :type  'list)

(defvar ryo.ui:after-tui-theme-loaded ()
  "Do some patches on TUI theme. ")

(cl-defun ryo.ui:tui-load-theme (&optional (theme ryo.ui:tui-theme))
  "Load theme for TUI. "
  (interactive)
  (load-theme theme t)
  (run-hooks 'ryo.ui:after-tui-theme-loaded))

(add-hook 'emacs-startup-hook #'ryo.ui:tui-load-theme)

;; tao-theme patch

(defun ryo.ui:tao-theme-tui-patch ()
  "Patches for `tao-theme'. "
  (set-face-italic-p 'font-lock-string-face nil nil)
  (set-face-italic-p 'font-lock-doc-face nil nil)
  (set-face-background 'default "undefined-bg" nil)

  ;; for visibility
  (set-face-attribute 'lsp-bridge-diagnostics-error-face nil
                      :background "orangered")
  (set-face-attribute 'lsp-bridge-diagnostics-hint-face nil
                      :background "gold")
  (set-face-attribute 'lsp-bridge-diagnostics-info-face nil
                      :background "orange")

  (set-face-attribute 'font-lock-comment-delimiter-face nil
                      :foreground "dimgrey"))

(add-hook 'ryo.ui:after-tui-theme-loaded #'ryo.ui:tao-theme-tui-patch)

;; blink-search patch for undefined-bg
;; this was done by patching `color-values' and give
;; it a fallback value if it returns `nil'

(defun ryo.ui:tui-patch-color-values (color-values)
  "If `color-values' return `nil', return `ryo.ui:tui-color-value-fallback' value. "
  (or color-values ryo.ui:tui-color-value-fallback))
(advice-add 'color-values :filter-return #'ryo.ui:tui-patch-color-values)

;; who don't like mouse manipulation?

(xterm-mouse-mode t)

;; turn off the menu bar

(menu-bar-mode -1)

(provide 'init-tui)

;;; init-tui.el ends here
