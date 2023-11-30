;;; init.el --- Emacs Configuration File -*- lexical-binding: t -*-
;;; Commentary:

;; Enable debug when encounter errors
(setf debug-on-error t)

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

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc)))

(use-package magit)

(use-package tramp)

(use-package eshell
  :config
  ;;; This is the defination of `ryo:eshell',
  ;;; which give some customization of my personal eshell.
  
  ;; require vc-git for `ryo:eshell-git'.
  (autoload 'vc-git-branches "vc-git")
  
  ;; custom variables
  (defcustom ryo:eshell-normal-emoji-sets
    '("[´･ᴗ･`]" "[´･ω･`]" "[ •_•]" "[•_• ]" "[੧ᐛ੭]" "[ง˙o˙]ว" "૧[●´৺`●]૭" "[ﾟ∀ﾟ*]")
    "A set of char displayed for normal prompt."
    :type 'list
    :group 'ryo:eshell)
  
  (defcustom ryo:eshell-error-emoji-sets
    '("ﾍ[´Д`]ﾉ" "[;◔౪◔]" "ε=ε=ヾ[;ﾟдﾟ]/" "[ﾟДﾟ≡ﾟдﾟ]" "[||ﾟДﾟ]" "[▼皿▼#]")
    "A set of char displayed for error prompt."
    :type 'list
    :group 'ryo:eshell)
  
  (defcustom ryo:eshell-remote-emoji-sets
    '("|ω・]")
    "A set of char displayed for remote status."
    :type 'list
    :group 'ryo:eshell)
  
  (defcustom ryo:eshell-path-name-shorten-trigger-length 30
    "The minimum path length to trigger `ryo:eshell-shorten-path-name'."
    :type 'list
    :group 'ryo:eshell)
  
  (defcustom ryo:eshell-path-name-maximum-length 80
    "The maximum path length after `ryo:eshell-shorten-path-name'."
    :type 'integer
    :group 'ryo:eshell)
  
  ;; custom faces
  (defface ryo:eshell-emoji-normal-face
    '((t :background "gold" :foreground "black"))
    "Eshell normal emoji prompt face."
    :group 'ryo:eshell)
  
  (defface ryo:eshell-emoji-error-face
    '((t :background "red" :foreground "white"))
    "Eshell error emoji prompt face."
    :group 'ryo:eshell)
  
  (defface ryo:eshell-remote-face
    '((t :foreground "red"))
    "Eshell remote face."
    :group 'ryo:eshell)
  
  (defface ryo:eshell-path-face
    '((t :foreground "dim gray"))
    "Eshell path face."
    :group 'ryo:eshell)
  
  (defface ryo:eshell-git-branch-face
    '((t :foreground "gray"))
    "Eshell git branch face."
    :group 'ryo:eshell)
  
  (defface ryo:eshell-prompt-face
    '((t :foreground "gold"))
    "Eshell prompt face."
    :group 'ryo:eshell)
  
  ;; custom functions
  (defun ryo:eshell-remote-p ()
    "Check if work remotely."
    (tramp-tramp-file-p default-directory))
  
  (defun ryo:eshell-git-branch ()
    "Return current git branch, if no, return `nil'."
    (let ((branch (car (vc-git-branches))))
      (if branch branch nil)))
  
  (defun ryo:eshell-shorten-path-name (path-name)
    "Try to make the `path-name' shorten than 
  `ryo:eshell-path-name-shorten-trigger-length'. "
    (let* ((path (split-string path-name "/"))
           (cnt  (1- (length path)))
           (len  (length path-name)))
      (string-join
       (cl-loop for rest = path then (cdr rest)
                for elem = (car rest)
                for i below cnt
                for name = (string-join
                            (mapcar (lambda (s) (substring s 0 1))
                                    (string-split elem "[-_\\. ]+" t))) 
                while (> len ryo:eshell-path-name-shorten-trigger-length)
                do (setf len (- len (length name)))
                collect name into shortened
                finally (return (append shortened rest)))
       "/")))
  
  (defun ryo:eshell-path ()
    "Return path of current working directory. 
  If the length of path name is longer `ryo:eshell-path-name-shorten-trigger-length',
  try to shorten the path name; and if the shortened path name is still longer than
  `ryo:eshell-path-name-maximum-length', try to cutoff the path name directly."
    (let ((path-name (abbreviate-file-name (eshell/pwd))))
      (if (length> path-name ryo:eshell-path-name-shorten-trigger-length)
          (let ((shortened (ryo:eshell-shorten-path-name path-name)))
            (if (length> shortened ryo:eshell-path-name-maximum-length)
                (concat "..."
                        (substring path-name
                                   (- (length shortened)
                                      ryo:eshell-path-name-maximum-length)
                                   (1- (length shortened))))
              shortened))
        path-name)))
  
  (defun ryo:luck-in (&optional posibility limit)
    "Return `t' or `nil' at `possibility' chance."
    (let ((p (or posibility 0.5))
          (lim (or limit 100)))
      (if (< (/ (random lim) (float lim)) p) t nil)))
  
  (defun ryo:pick (lst)
    "Pick random element from `lst'."
    (cl-loop for elem in lst
             for count from (length lst) downto 1
             if (ryo:luck-in (/ 1.0 count)) return elem))
  
  (defmacro ryo:with-face (string face)
    "Bind `string' with `face'."
    `(propertize ,string 'face ,face))
  
  (defun eshell-previous-matching-input-from-input (arg)
    "Search backwards through input history for match for current input.
  \(Previous history elements are earlier commands.)
  With prefix argument N, search for Nth previous match.
  If N is negative, search forwards for the -Nth following match.
  
  This is a patch for `eshell-previous-matching-input-from-input' which fails
  to match if you're using custom prompt that change every time."
    (interactive "p")
    (if (not (memq last-command '(eshell-previous-matching-input-from-input
          eshell-next-matching-input-from-input)))
        ;; Starting a new search
        (setq eshell-matching-input-from-input-string
        (buffer-substring (save-excursion (eshell-bol) (point))
              (point))
        eshell-history-index nil))
    (eshell-previous-matching-input
     (concat eshell-prompt-regexp
             (regexp-quote
              (replace-regexp-in-string
               eshell-prompt-regexp ""
               eshell-matching-input-from-input-string)))
     arg))
  
  (defun ryo:eshell-emoji-prompt ()
    "Eshell emoji prompt."
    (setf eshell-prompt-regexp "^[^#>]* [#>] ")
    (concat
     ;; first line: remote path git
     (when (ryo:eshell-remote-p)
       (concat (ryo:with-face (ryo:pick ryo:eshell-remote-emoji-sets)
                              'ryo:eshell-remote-face)
               " "))
     (ryo:with-face (ryo:eshell-path) 'ryo:eshell-path-face)
     (let ((branch (ryo:eshell-git-branch)))
       (when branch
         (concat " " (ryo:with-face branch 'ryo:eshell-git-branch-face))))
     "\n"
  
     ;; second line: status-emoji prompt
     (if (zerop eshell-last-command-status)
         (ryo:with-face (ryo:pick ryo:eshell-normal-emoji-sets)
                        'ryo:eshell-emoji-normal-face)
       (ryo:with-face (ryo:pick ryo:eshell-error-emoji-sets)
                      'ryo:eshell-emoji-error-face))
  
     (ryo:with-face (if (= (user-uid) 0) " #" " >") 'ryo:eshell-prompt-face)
     " "))
  (setf eshell-prompt-function #'ryo:eshell-emoji-prompt))

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

(use-package irony
  :hook (((c++-mode c-mode) . irony-mode)))

(use-package flycheck-irony
  :after (irony))

(use-package irony-eldoc
  :hook ((irony-mode . irony-eldoc))
  :after (irony))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp))

(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-java
  :hook ((java-mode . lsp))
  :after (lsp)
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (setq c-default-style "java"
                    c-basic-offset 2
                    tab-width 2
                    indent-tabs-mode nil)
              (c-set-offset 'arglist-intro '+)
              (c-set-offset 'arglist-close '0)
              (c-set-offset 'case-label '+))))

(use-package org
  :config
  ;; org-mode and babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp       . t)
     (dot        . t)
     (C          . t)
     (python     . t)
     (ruby       . t)
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
  (setf (plist-get org-format-latex-options :scale) 1.6)
  ;; set default latex compiler to xelatex
  (setf org-latex-compiler "xelatex")
  
  ;; add xeCJK for Chinese support
  (add-to-list 'org-latex-packages-alist
               '("" "xeCJK" t ("xelatex")))
  )

;;; Use CDLaTeX for quick LaTeX equation input
(use-package cdlatex
  :hook ((org-mode . turn-on-org-cdlatex)))

(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :config
  (setq separedit-default-mode 'org-mode))

(use-package ox-pandoc)

(use-package ox-reveal
  :config
  (setq-default org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

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
