;;; init-tui.el --- Init for TUI.

;; use tao-yang as theme, prevent blink-search error
;; see https://github.com/manateelazycat/blink-search/issues/16

(require 'tao-theme)
(require 'tao-yang-theme)
(load-theme 'tao-yang t)

;; who don't like mouse manipulation?

(xterm-mouse-mode t)

;; turn off the menu bar

(menu-bar-mode -1)

(provide 'init-tui)

;;; init-tui.el ends here
