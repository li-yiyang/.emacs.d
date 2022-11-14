;;; init-lookings.el --- Emacs Lookings -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://www.linw1995.com/en/blog/Using-Emacs-Editor-For-The-First-Time/

;;; Code:

;; Dashboard: https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
             :config
             (dashboard-setup-startup-hook)
             (setq dashboard-center-content t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(use-package solarized-theme
  :config
  (load-theme 'solarized-selenized-black))

;; switching between dark and light mode in macOS
;; from http://sodaware.sdf.org/notes/emacs-darkmode-theme-switch/
(defun match-emacs-theme-to-system ()
  "Automatically set the theme to match if OSX is in dark mode."
  (interactive)

  (if (system-dark-mode-enabled-p)
      (load-theme 'solarized-selenized-black  t)
      (load-theme 'solarized-selenized-white t)))

(defun system-dark-mode-enabled-p ()
  "Check if dark mode is currently enabled on OSX."
  (if (string= system-type "darwin")
      (string=
       (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"")
       "true")
      nil))

; (match-emacs-theme-to-system)

;; auto disable theme when switching
;; from https://emacs-china.org/t/topic/15158/4
(defcustom load-theme-before-hook nil
  "Functions to run before load theme."
  :type 'hook)

(defcustom load-theme-after-hook nil
  "Functions to run after load theme."
  :type 'hook)

(defun load-theme-hook-wrapper (origin-func theme &rest args)
  "A wrapper of hooks around `load-theme'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'load-theme-before-hook theme)
  (apply origin-func theme args)
  (run-hook-with-args 'load-theme-after-hook theme))

(advice-add 'load-theme :around #'load-theme-hook-wrapper)

;   (run-with-timer 3600 nil 'update-theme))

(provide 'init-lookings)
;;; init-lookings.el ends here
