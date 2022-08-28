;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files, inspired by Purcell's config.
;; Also, https://nyk.ma/posts/emacs-write-your-own/ is another good ref.

;; Note: The Evil Mode is defaultly switched on, if you'd like to disable it,
;; you could change the *enable-evil-mode* to nil.

;;; Code:

;; Produce backtraces when errors occur: (for debug)
;; (setq debug-on-error)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *enable-evil-mode* t)

(defconst *is-macos* (eq system-type 'darwin))

;; Basics
(tool-bar-mode -1) ;; remove tool bar
(scroll-bar-mode -1) ;; remove scroll bar
(delete-selection-mode t) ;; delete selected text
(electric-pair-mode t) ;; auto complete pairs

;; Disable auto backups
(setq make-backup-files nil)

;; Mouse Wheeling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; enable opacity for macOS
(when *is-macos*
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist
               '(alpha . (99 . 99))))

;; Network...
;; (setq url-proxy-services
;;   '(("http" . "127.0.0.1:7890")
;;     ("https" . "127.0.0.1:7890")))


(require 'init-keybinding) ;; Basic Key Binding

;; Calls (package-initialize)
(require 'init-elpa) ;; Package Manager

(require 'init-lookings) ;; Emacs Lookings
(require 'init-minibuffer-complete) ;; Minibuffer Complete
(require 'init-auto-complete) ;; Code Auto Complete
(require 'init-error-warning) ;; Errors and Warning Engine
(require 'init-lsp) ;; LSP
(require 'init-yasnippet) ;; Template
(require 'init-org) ;; Org Mode
(require 'init-paredit) ;; Paredit
(require 'init-file-manager) ;; File Manager
(require 'init-window-manager) ;; Window Manager
(if *is-macos*   ;; Terminal in Emacs
    (require 'init-terminal))

;; Conditional Package Loading
(if *enable-evil-mode*
 (require 'init-evil)) ;; Evil Mode

; (require 'init-proj-man) ;; Project Manager

;; Not many configuration package
(use-package magit) ;; Git
(use-package anzu)  ;; regexp search and replace
(use-package ctrlf  ;; Searching C-s to search
  :config
  (ctrlf-mode t))

;; Basics of All the Language
; (use-package rainbow-delimiters)
; (use-package paredit)


;; Language
(require 'init-ruby) ;; Ruby
(require 'init-racket) ;; Racket
(require 'init-latex) ;; Latex
(require 'init-markdown) ;; Markdown
; (require 'init-c) ;; C
; (require 'init-...

;; Custom configuration
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
