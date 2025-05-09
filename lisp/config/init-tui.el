;;; init-tui.el --- Init for TUI. -*- lexical-binding: t -*-

;; use tao-yang as theme, prevent blink-search error
;; see https://github.com/manateelazycat/blink-search/issues/16

(require 'tao-theme)
(require 'tao-yang-theme)
(require 'tao-yin-theme)

(defcustom ryo.ui:tui-theme 'tao-yang
  "The default TUI theme. "
  :group 'ryo.ui)

(defcustom ryo.ui:tui-color-value-fallback
  (color-values "#FFFFFF")
  "The color value fallback for `color-values'.
see \\=`ryo.ui:tui-patch-color-values\\='. "
  :group 'ryo.ui
  :type  'list)

(defvar ryo.ui:after-tui-theme-loaded-hook ()
  "Do some patches on TUI theme. ")

(cl-defun ryo.ui:tui-load-theme (&optional (theme ryo.ui:tui-theme))
  "Load theme for TUI. "
  (interactive "STheme: \n")
  (mapc #'disable-theme custom-enabled-themes) ; unload all
  (load-theme theme t)
  (run-hooks 'ryo.ui:after-tui-theme-loaded-hook))

(add-hook 'emacs-startup-hook #'ryo.ui:tui-load-theme)

(defun ryo.ui:dark-theme ()
  "Dark! Duck! "
  (interactive)
  (ryo.ui:tui-load-theme 'tao-yin))

(defun ryo.ui:light-theme ()
  "Light! Bright! "
  (interactive)
  (ryo.ui:tui-load-theme 'tao-yang))

;; tao-theme patch

(defun ryo.ui:tao-theme-tui-patch ()
  "Patches for `tao-theme'. "
  (set-face-italic-p 'font-lock-string-face nil nil)
  (set-face-italic-p 'font-lock-doc-face nil nil)
  (set-face-background 'default "undefined-bg" nil)

  (set-face-attribute 'font-lock-comment-delimiter-face nil
                      :foreground "dimgrey")

  (set-face-background 'tty-menu-disabled-face "undefined-bg" nil)
  (set-face-background 'tty-menu-enabled-face  "undefined-bg" nil)
  (set-face-attribute  'tty-menu-enabled-face nil
                       :foreground "dimgrey")
  (set-face-attribute  'tty-menu-disabled-face nil
                       :foreground "dimgrey"))

(add-hook 'ryo.ui:after-tui-theme-loaded-hook #'ryo.ui:tao-theme-tui-patch)

;; blink-search patch for undefined-bg
;; this was done by patching `color-values' and give
;; it a fallback value if it returns `nil'

(defun ryo.ui:tui-patch-color-values (color-values)
  "If `color-values' return `nil', return `ryo.ui:tui-color-value-fallback' value. "
  (or color-values ryo.ui:tui-color-value-fallback))
(advice-add 'color-values :filter-return #'ryo.ui:tui-patch-color-values)

;; who don't like mouse manipulation?

(xterm-mouse-mode t)

;; turn on the menu bar

(menu-bar-mode 1)

(provide 'init-tui)

;;; init-tui.el ends here
