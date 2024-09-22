;;; init-tui.el --- Init for TUI.

;; use tao-yang as theme, prevent blink-search error
;; see https://github.com/manateelazycat/blink-search/issues/16

(require 'tao-theme)
(require 'tao-yang-theme)

(defcustom ryo.ui:tui-theme 'tao-yang
  "The default TUI theme. "
  :group 'ryo.ui)

(cl-defun ryo.ui:tui-load-theme (&optional (theme ryo.ui:tui-theme))
  "Load theme for TUI. "
  (load-theme theme t))

(add-hook 'emacs-startup-hook #'ryo.ui:tui-load-theme)

;; who don't like mouse manipulation?

(xterm-mouse-mode t)

;; turn off the menu bar

(menu-bar-mode -1)

(provide 'init-tui)

;;; init-tui.el ends here
