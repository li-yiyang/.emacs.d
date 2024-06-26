;;; init.el --- Emacs Configuration File -*- lexical-binding: t -*-
;;; Commentary:

;; Enable debug when encounter errors
(setf debug-on-error nil)
(setf warning-minimum-level :error)

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

(load-file (expand-file-name "pre-custom.el" user-emacs-directory))

(require 'package)
(package-initialize)

;; not refresh every time for quicker start up time
;; (package-refresh-contents)

;; use-package was built in after emacs 29
(when (version< emacs-version "29")
  (package-install 'use-package))

(require 'use-package)

;; Always ensure the package
(setq use-package-always-ensure t)

;; Note: this is a dirty patch to use enhanced latex preview org mode
;; I should change this in the future, but this did make the org mode
;; load before emacs org. 
(use-package org
  :load-path "org-mode/lisp"
  :config
  ;; org-mode and babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp       . t)     
     (dot        . t)
     (C          . t)
     (python     . t)    
     (shell      . t)
     (gnuplot    . t)))
  
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
  
  ;; Pretty symbol for org-mode
  (defvar ryo:org-prettify-symbols-alist
    '(("\\begin"  . ?▼)
      ("\\end"    . ?▲)
      ("\\mapsto" . ?↦)
      ("\\frac"   . ?𝐟)
      ("\\sqrt"   . ?√)
      ("\\updownarrow" . ?↕)
      ("\\boldsymbol" . ?𝐛)
      ("\\mathbb" . ?𝐁)
      ("\\mathrm" . ?𝐫)
      ("\\mathcal" . ?𝐜)
      ("\\left\\Vert" . ?‖)
      ("\\right\\Vert" . ?‖)))
  
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
  (add-to-list 'org-latex-packages-alist
               '("" "amsmath" t ("xelatex")))
  (add-to-list 'org-latex-packages-alist
               '("" "amsfonts" t ("xelatex")))
  
  ;; add listing for LaTeX code block
  (setf org-latex-listings 'listings)
  (setf org-latex-custom-lang-environments
        '((python "pythoncode")
          (lisp   "common-lispcode")
          (elisp  "common-lispcode")))
  (setf org-latex-listings-options
        '(("showspaces" "false")
          ("breaklines" "true")
          ("keepspaces" "true")
          ("showstringspaces" "false")
          ("basicstyle" "\\ttfamily")
          ("numbers" "left")
          ("numberstyle" "\\footnotesize")
          ("frame" "single")
          ("captionpos" "b")        
          ("extendedchars" "true")))
  
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  
  (setq org-latex-listings-options
        '(("frame" "lines")
          ("basicstyle" "\\footnotesize")
          ("numbers" "left")
          ("numberstyle" "\\tiny")))
  
  ;; org-mode latex-preview enable
  (plist-put (cdr (assoc 'dvisvgm org-latex-preview-process-alist))
             :image-converter
             '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f"))
  )

(use-package yasnippet
  :load-path "yasnippet"
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

(use-package puni
  :load-path "puni"
  :config
  (puni-global-mode))

(use-package atomic-chrome
  :load-path "atomic-chrome"
  :config
  (atomic-chrome-start-server)
  (setf atomic-chrome-default-major-mode 'markdown-mode))

(use-package blink-search
  :load-path "blink-search"
  :bind ("C-s" . blink-search)
  :custom ((blink-search-enable-posframe       t)
           (blink-search-posframe-standalone   nil)
           (blink-search-posframe-width-ratio  0.8)
           (blink-search-posframe-height-ratio 0.6))
  :config
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

(use-package magit
  :load-path "magit")

(use-package emojishell
  :load-path "emojishell"
  :config
  (setf eshell-prompt-function #'emojishell-emoji-prompt))

(use-package dirvish
  :load-path "dirvish"
  :config
  (dirvish-override-dired-mode))

(use-package sly
  :load-path "sly"
  :config
  (require 'sly-autoloads)
  
  ;; org-mode babel
  (setf org-babel-lisp-eval-fn #'sly-eval)

  ;; sbcl with larger dynamic space size
  (setf inferior-lisp-program
        '("sbcl" "--dynamic-space-size" "4096" "--control-stack-size" "24"))

  (defvar ryo:sly-keymap-function-bind
    `(("C-l v"   'sly-describe-symbol)
      ("C-l f"   'sly-describe-function)
      ("C-l c"   'sly-who-calls)
      ("C-l b"   'sly-who-binds)
      ("C-l C-l" 'recenter-top-bottom))
    "The key binding for sly. ")

  ;; add sly-mrepl hook for C-return
  (defun ryo:register-sly-mrepl-key-map ()
    (require 'sly-mrepl)
    (define-key sly-mrepl-mode-map (kbd "RET")        nil)
    (define-key sly-mrepl-mode-map (kbd "C-<return>") #'sly-mrepl-return)
    (define-key sly-mrepl-mode-map (kbd "S-<return>") #'sly-mrepl-return))
  (add-hook 'sly-mrepl-mode-hook #'ryo:register-sly-mrepl-key-map)
  (add-hook 'sly-mrepl-mode-hook #'electric-pair-local-mode)

  ;; some help keys
  (add-hook 'sly-mrepl-mode-hook #'electric-pair-local-mode)
  (define-key lisp-mode-map (kbd "C-l v")   #'sly-describe-symbol)
  (define-key lisp-mode-map (kbd "C-l f")   #'sly-describe-function)
  (define-key lisp-mode-map (kbd "C-l c")   #'sly-who-calls)
  (define-key lisp-mode-map (kbd "C-l b")   #'sly-who-binds)
  (define-key lisp-mode-map (kbd "C-l C-l") #'recenter-top-bottom))

(use-package company
  :hook (((lisp-mode sly-mrepl-mode) . company-mode)))

;; company with posframe for better UI
(use-package company-posframe
  :hook (((company-mode) . company-posframe-mode)))

(use-package lsp-bridge
  :load-path "lsp-bridge"
  :custom ((lsp-bridge-enable-org-babel t)
           (lsp-bridge-enable-hover-diagnostic t))
  :config

  ;; acm key binding
  (define-key acm-mode-map [tab] nil)
  (define-key acm-mode-map "\t"  nil)

  (global-lsp-bridge-mode))

;; Ensure Python Command
(setf org-babel-python-command
      (cond ((executable-find "python3") "python3")
            ((executable-find "python2") "python2")
            (t "python")))

(setf python-indent-guess-indent-offset nil)

(use-package verilog-mode)

(use-package verilog-ext
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  (setf verilog-ext-feature-list
        '(font-lock xref capf hierarchy
          lsp flycheck beautify navigation template
          formatter compilation imenu which-func hideshow
          typedefs time-stamp block-end-comments ports))
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

(use-package org-appear
  :load-path "org-appear"
  :after org
  :hook ((org-mode . org-appear-mode))
  :config
  (setf org-hide-emphasis-markers t
        org-appear-autolinks      nil
        org-pretty-entities       t
        org-appear-autoentities   t
        org-appear-autokeywords   t))

;;; Use CDLaTeX for quick LaTeX equation input
(use-package cdlatex
  :load-path "cdlatex"
  :hook ((org-mode . turn-on-org-cdlatex))
  :config
  (setf cdlatex-math-modify-alist
        '(( ?b "\\boldsymbol" "\\textbf" t nil nil )
          ( ?B "\\mathbb"     "\\textbf" t nil nil))))

;;; AUCTeX
(use-package tex
  :load-path "auctex"
  :ensure auctex)

(use-package ebib
  :load-path "ebib"
  :bind ("C-c e" . ebib)
  :config
  ;; support for ebib link
  (org-link-set-parameters
   "ebib"
   :follow #'org-ebib-open
   :store  #'org-ebib-store-link
   :export (lambda (&rest args)
             (apply #'org-ref-cite-export (cons "cite" args))))

  (setf (cdr (assoc 'org-mode ebib-citation-commands))
        '((("ebib" "[[ebib:%K]]")))))

(use-package ox-pandoc
  :load-path "ox-pandoc")

(use-package valign
  :load-path "valign"
  :hook ((org-mode . valign-mode)))

(use-package markdown-mode
  :load-path "markdown-mode"
  :config
  ;; hide markups symbols and urls for better lookings
  (setq-default markdown-hide-markup nil
                markdown-hide-urls   t)

  ;; show code colorized
  (setq-default markdown-fontify-code-blocks-natively t))

(use-package separedit
  :load-path "separedit"
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :config
  (setq separedit-default-mode 'text-mode))

(load-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init)
;; init.el ends here
