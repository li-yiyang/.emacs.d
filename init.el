;;; init.el --- Emacs Configuration File -*- lexical-binding: t -*-
;;; Commentary:

;; Enable debug when encounter errors
(setf debug-on-error t)

(when (eq system-type 'darwin)
  ;; font scale
  (set-face-attribute 'default nil :height 140)

  ;; Set for how default frame looks like
  (setq default-frame-alist '((menu-bar-lines . nil)
                              (tool-bar-lines . nil)
                              (vertical-scroll-bars . nil)))

  ;; pixel scroll mode
  (pixel-scroll-precision-mode t)

  ;; I am using yabai
  (when (executable-find "yabai")
    (setf frame-resize-pixelwise t)
    (add-to-list 'default-frame-alist '(undecorated . t))))

(when (eq system-type 'gnu/linux)
  ;; font scale
  (set-face-attribute 'default nil :height 130)

  ;; set for default frame
  (setq default-frame-alist '((menu-bar-lines . nil)
                              (tool-bar-lines . nil)))

  ;; pixel scroll mode
  (pixel-scroll-precision-mode))

;; Use space by default instead of tab for indention
(setq-default indent-tabs-mode nil)

;; Use 2 whitespace for indention rather than tabs
(setq-default tab-width 2)

(defvar ryo:autosaves-dir
  (expand-file-name "autosaves" user-emacs-directory)
  "All autosaves files goes into `ryo:autosaves-dir'.")

;; make `ryo:autosaves-dir' if it's not exists
(unless (file-exists-p ryo:autosaves-dir)
  (make-directory ryo:autosaves-dir t))

(setf auto-save-file-name-transforms
      `(("\(?:[^/]/\)\(.*\)" ,(expand-file-name "\\1" ryo:autosaves-dir) t)))

(setf auto-save-default t
      auto-save-timeout 10   ; second
      auto-save-interval 100 ; after 233 input events, trigger autosave
      )

(defvar ryo:backups-dir
  (expand-file-name "backups" user-emacs-directory)
  "All backup file goes into `ryo:backups-dir'.")

;; make `ryo:backups-dir' if it's not exists
(unless (file-exists-p ryo:backups-dir)
  (make-directory ryo:backups-dir t))

;; All backups shall be stored into `backups' folder
(setf backup-directory-alist `((".*" . ,ryo:backups-dir)))

(require 'package)
(package-initialize)

;; Use emacs-china mirror for better connection
(setq package-archives '(("gnu" . "http://1.15.88.122/gnu/")
                         ("melpa" . "http://1.15.88.122/melpa/")
                         ("nongnu" . "http://1.15.88.122/nongnu/")
                         ("org" . "http://1.15.88.122/org/")))

;; not refresh every time for better start up time
;; (package-refresh-contents)

;; use-package was built in after emacs 29
(when (version< emacs-version "29")
  (package-install 'use-package))

(require 'use-package)

;; Always ensure the package
(setq use-package-always-ensure t)

(use-package company
  :config
  (global-company-mode))

(use-package magit)

(use-package paredit
  :hook ((emacs-lisp-mode
          lisp-mode
          ielm-mode
          eval-expression-minibuffer-setup)
         . enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("C-<return>" . ryo:paredit-C-RET))
  :init
  (defvar ryo:paredit-C-RET-probe-method '()
    "Probes to trigger corresponding methods.

The elements of `ryo:paredit-C-RET-probe-method' should be a pair like:

  (probe-function-to-trigger . methods-responding)

for example: 

  (#'minibufferp . #'read--expression-try-read)")

  :config
  (defun ryo:paredit-C-RET ()
    "Trigger by `ryo:paredit-C-RET-probe-method', fall back to `paredit-RET'."
    (interactive)
    (cl-loop for (trigger . method) in ryo:paredit-C-RET-probe-method
             if (or (and (symbolp trigger) (eq major-mode trigger))
                    (and (functionp trigger) (funcall trigger)))
             do (cl-return (funcall method))
             finally (paredit-RET)))

  ;; eval-expression-minibuffer-setup
  (add-to-list 'ryo:paredit-C-RET-probe-method
               (cons #'minibufferp #'read--expression-try-read))
  ;; ielm
  (add-to-list 'ryo:paredit-C-RET-probe-method
               (cons 'inferior-emacs-lisp-mode #'ielm-return)))

(use-package sly
  :hook ((sly-mrepl-mode . enable-paredit-mode))
  :config
  (require 'sly-autoloads)

  ;; org-mode babel
  (setf org-babel-lisp-eval-fn #'sly-eval)

  ;; sbcl with larger dynamic space size
  (setf inferior-lisp-program '("sbcl" "--dynamic-space-size" "4GB"))

  ;; ryo/paredit-C-RET
  (add-to-list 'ryo:paredit-C-RET-probe-method
               (cons 'sly-mrepl-mode #'sly-mrepl-return)))

(use-package sly-quicklisp
  :after '(sly))

(use-package org
  :config
  ;; org-mode and babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)))

  ;; auto display image after babel eval
  (defun ryo:org-babel-display-image-after-eval ()
    "Switch on auto image display after babel eval."
    (interactive)
    (add-hook 'org-babel-after-execute-hook #'org-display-inline-images))

  (defun ryo:org-babel-no-display-image-after-eval ()
    "Swtich off auto image display after babel eval."
    (interactive)
    (remove-hook 'org-babel-after-execute-hook #'org-display-inline-images))

  (ryo:org-babel-display-image-after-eval) ; default on

  ;; org-mode and image preview
  (setf org-image-actual-width nil)

  ;; org-mode and latex preview
  ;; org pretty symbol for raw input
  (setf org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil)

  ;; Use dvisvgm for SVG preview
  (setf org-preview-latex-default-process 'dvisvgm)
  (setf (plist-get org-format-latex-options :scale) 1.6))

;;; Use CDLaTeX for quick LaTeX equation input
(use-package cdlatex
  :hook ((org-mode . turn-on-org-cdlatex)))

(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :config
  (setq separedit-default-mode 'org-mode))

(use-package markdown-mode
  :config
  ;; hide markups symbols and urls for better lookings
  (setq-default markdown-hide-markup t
                markdown-hide-urls   t)

  ;; show code colorized
  (setq-default markdown-fontify-code-blocks-natively t))

(use-package markdown-toc
  :after (markdown-mode)
  :config
  (defun ryo:add-markdown-toc-hook-before-save ()
    "Add locally hook for `before-save-hook'."
    (add-hook 'before-save-hook #'markdown-toc-generate-or-refresh-toc 0 'local))
  (add-hook 'markdown-mode-hook #'ryo:add-markdown-toc-hook-before-save))

(use-package gnuplot)
