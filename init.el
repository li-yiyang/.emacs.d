;;; init.el --- Emacs Configuration File -*- lexical-binding: t -*-
;;; Commentary:

;; Enable debug when encounter errors
(setf debug-on-error nil)
(setf warning-minimum-level :error)

(when (eq system-type 'darwin)
  ;; font scale and fonts
  (cond
   ((find-font (font-spec :family "Sarasa Term SC"))
    (set-face-attribute 'default nil
                         :family "Sarasa Term SC"
                         :height 160))
   (t (set-face-attribute :height 130)))

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

(defun ryo:ensure-dir (path)
  "Make sure `path' is existing."
  (unless (file-exists-p path)
    (make-directory path t)))

(defvar ryo:autosaves-dir
  (expand-file-name "autosaves" user-emacs-directory)
  "All autosaves files goes into `ryo:autosaves-dir'.")

;; make `ryo:autosaves-dir' if it's not exists
(ryo:ensure-dir ryo:autosaves-dir)

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
(ryo:ensure-dir ryo:backups-dir)

;; All backups shall be stored into `backups' folder
(setf backup-directory-alist `((".*" . ,ryo:backups-dir)))

(require 'package)
(package-initialize)

;; Use emacs-china mirror for better connection
(setq package-archives '(("gnu"    . "http://1.15.88.122/gnu/")
                         ("melpa"  . "http://1.15.88.122/melpa/")
                         ("nongnu" . "http://1.15.88.122/nongnu/")
                         ("org"    . "http://1.15.88.122/org/")))

;; not refresh every time for quicker start up time
;; (package-refresh-contents)

;; use-package was built in after emacs 29
(when (version< emacs-version "29")
  (package-install 'use-package))

(require 'use-package)

;; Always ensure the package
(setq use-package-always-ensure t)

(use-package yasnippet
  :config
  (defvar ryo:yas-snippet-path
    (expand-file-name "snippets" user-emacs-directory)
    "Path of snippet dirs (synced with git).")

  (defvar ryo:yas-snippet-path-local-only
    (expand-file-name "local-snippets" user-emacs-directory)
    "Path of local only snippets (not synced with git).")

  (ryo:ensure-dir ryo:yas-snippet-path)
  (ryo:ensure-dir ryo:yas-snippet-path-local-only)

  (setq yas-snippet-dirs
        (list ryo:yas-snippet-path
              ryo:yas-snippet-path-local-only))

  (yas-global-mode 1))

(electric-pair-mode 1)

(use-package puni
  :config
  (puni-global-mode))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setf atomic-chrome-default-major-mode 'markdown-mode))

(use-package blink-search
  :load-path "blink-search"
  :bind ("C-s" . blink-search)
  :custom ((blink-search-enable-posframe       t)
           (blink-search-posframe-standalone   nil)
           (blink-search-posframe-width-ratio  0.8)
           (blink-search-posframe-height-ratio 0.6)
           (blink-search-search-backends       '("Buffer List"
                                                 "Current Buffer"
                                                 "Find File"
                                                 "Elisp Symbol")))
  :cofig
  ;; Note: change how the posframe is shown
  (defun blink-search-posframe-show (buffer)
    (let* ((posframe-height (round (* (frame-height) blink-search-posframe-height-ratio)))
           (posframe-width  (round (* (frame-width) blink-search-posframe-width-ratio))))
      (apply #'posframe-show
             (get-buffer buffer)
             :poshandler #'posframe-poshandler-frame-bottom-center
             (list
              :max-height (frame-height)
              :min-height posframe-height
              :min-width  posframe-width
              :max-width  (frame-width)
              :border-width 2
              :border-color "gray"
              :accept-focus (equal buffer blink-search-input-buffer)
              )))))

(use-package magit)

(use-package tramp)

(use-package emojishell
  :load-path "emojishell"
  :config
  (setf eshell-prompt-function #'ryo:eshell-emoji-prompt))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

(use-package sly
  :config
  (require 'sly-autoloads)

  ;; org-mode babel
  (setf org-babel-lisp-eval-fn #'sly-eval)

  ;; sbcl with larger dynamic space size
  (setf inferior-lisp-program '("sbcl" "--dynamic-space-size" "4GB"))

  ;; add sly-mrepl hook for C-return
  (add-hook 'sly-mrepl-hook
            (lambda () (local-set-key (kbd "C-<return>") #'sly-mrepl-return))))

(use-package sly-quicklisp
  :after '(sly)
  :config
  (add-to-list 'sly-contribs 'sly-quicklisp 'append))

(use-package sly-asdf
  :after '(sly)
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package company
  :hook (((lisp-mode sly-mrepl-mode) . company-mode)))

;; company with posframe for better UI
(use-package company-posframe
  :hook (((company-mode) . company-posframe-mode)))

(use-package irony
  :hook (((c++-mode c-mode) . irony-mode)
         ((c++-mode c-mode) . lsp)))

(use-package flycheck-irony
  :after (irony))

(use-package irony-eldoc
  :hook ((irony-mode . irony-eldoc))
  :after (irony))

(use-package lsp-bridge
  :load-path "lsp-bridge"
  :config
  (setf lsp-bridge-enable-org-babel t)

  ;; acm key binding
  (define-key acm-mode-map (kbd "TAB")  nil)

  (global-lsp-bridge-mode))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (ruby-mode . lsp)))

(use-package rvm)

;; Ensure Python Command
(setf org-babel-python-command
      (cond ((executable-find "python3") "python3")
            ((executable-find "python2") "python2")
            (t "python")))

;; emacs-jupyter manual installation
(use-package simple-httpd)

(use-package zmq)

(use-package jupyter
  :load-path "jupyter")

(use-package code-cells
  :custom ((code-cells-convert-ipynb-style
            '(("pandoc" "--to" "ipynb" "--from" "org")
              ("pandoc" "--to" "org" "--from" "ipynb")
              (lambda () #'org-mode)))))

(setf python-indent-guess-indent-offset nil)

(use-package cern-root-mode
  :config
  (setf cern-root-filepath (executable-find "root")))

(use-package verilog-mode)

(use-package verilog-ext
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  (setf verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          lsp
          flycheck
          beautify
          navigation
          template
          formatter
          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          block-end-comments
          ports))
  :config
  (defconst verilog-ext-block-end-keywords-complete-re
    (concat
     ; Blanks and block end keyword
     "^\\(?1:\\s-*" verilog-ext-block-end-keywords-re "\\)\\s-*"
     ; Comments
     ":\\s-*\\(\\(block:\\|" verilog-identifier-sym-re "\\s-*::\\)\\s-*\\)*"
     ; Block name to be replaced
     "\\(?2:" verilog-identifier-sym-re "\\)\\s-*$"))                 

  (defun verilog-ext-block-end-comments-to-names ()
    "Convert valid block-end comments to ': BLOCK_NAME'.

Examples: endmodule // module_name             → endmodule : module_name
          endfunction // some comment          → endfunction // some comment
          endfunction // class_name::func_name → endfunction : func_name
          end // block: block_name             → end : block_name"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward verilog-ext-block-end-keywords-complete-re nil :noerror)
        (when (not (member (match-string-no-properties 2) verilog-keywords))
          (replace-match "\\1 // \\2")))))

  (verilog-ext-mode-setup))

(use-package swift-mode
  :hook ((swift-mode . lsp)))

(use-package lsp-sourcekit
  :after (lsp-mode)
  :config
  (when (eq system-type 'darwin)
    (setf lsp-sourcekit-executable
          (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))))

(use-package org
  :config
  ;; org-mode and babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp       . t)
     (org        . t)
     (dot        . t)
     (C          . t)
     (python     . t)
     (ruby       . t)
     (shell      . t)
     (gnuplot    . t)
     (jupyter    . t)))
  
  ;; emacs-jupyter patch
  (org-babel-jupyter-override-src-block "python")
  
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
  (setf (plist-get org-format-latex-options :scale) 1.6)
  
  ;; Pretty symbol for org-mode
  (defvar ryo:org-prettify-symbols-alist
    '(("\\begin"  . ?▼)
      ("\\end"    . ?▲)
      ("\\mapsto" . ?↦)
      ("\\frac"   . ?𝐟)
      ("\\sqrt"   . ?√)
      ("\\updownarrow" . ?↕)
      ("\\boldsymbol" . ?𝐛)
      ("\\mathrm" . ?𝐫)
      ("\\mathcal" . ?𝐜)))
  
  (defun ryo:setup-org-pretty-symbol-mode ()
    (setq-local prettify-symbols-alist
                (append prettify-symbols-alist
                        ryo:org-prettify-symbols-alist))
    (prettify-symbols-mode t))
  
  (add-hook 'org-mode-hook #'ryo:setup-org-pretty-symbol-mode)
  ;; set default latex compiler to xelatex
  (setf org-latex-compiler "xelatex")
  
  ;; add xeCJK for Chinese support
  (add-to-list 'org-latex-packages-alist
               '("" "xeCJK" t ("xelatex")))
  )

;;; Use CDLaTeX for quick LaTeX equation input
(use-package cdlatex
  :hook ((org-mode . turn-on-org-cdlatex))
  :config
  (setf cdlatex-math-modify-alist
        '(( ?b "\\boldsymbol" "\\textbf" t nil nil ))))

;;; AUCTeX
(use-package tex
  :ensure auctex)

(use-package epc)

(use-package pix2tex-el
  :load-path "pix2tex-el"
  :config
  (defun ryo:turn-on-pix2tex-el-in-org-mode ()
    (local-set-key (kbd "C-c l") #'pix2tex-el-insert)
    (add-hook 'pix2tex-el-insert-hook #'org-latex-preview 0 nil))
  (add-hook 'org-mode-hook #'ryo:turn-on-pix2tex-el-in-org-mode))

(use-package ebib
  :bind ("C-c e" . ebib)
  :config
  ;; support for ebib link
  (org-link-set-parameters
   "ebib"
   :follow #'org-ebib-open
   :store  #'org-ebib-store-link
   :export (lambda (&rest args)
             (apply #'org-ref-cite-export (cons "cite" args))))

  (setf org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "biber %b"                ; make sure to use bibtex backend
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f"))
  (setf (cdr (assoc 'org-mode ebib-citation-commands))
        '((("ebib" "[[ebib:%K]]")))))

(use-package biblio
  :config
  (require 'ebib-biblio))

;;; Note: in my machine, org-ref will arise signature failure
(setf package-check-signature nil)

(use-package org-ref)

(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :config
  (setq separedit-default-mode 'text-mode))

(use-package ox-pandoc)

(use-package org-appear
  :hook ((org-mode . org-appear-mode))
  :custom ((org-hide-emphasis-markers t)
           (org-appear-autolinks      nil)
           (org-pretty-entities       t)
           (org-appear-autoentities   t)
           (org-hidden-keywords       t)
           (org-appear-autokeywords   t)
           (org-appear-inside-latex   t)))

(use-package olivetti
  :custom ((olivetti-body-width   82)
           (olivetti-margin-width 0)))

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

(use-package doc-view
  :config
  (setf doc-view-resolution 400))
