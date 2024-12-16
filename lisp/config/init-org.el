;;; init-org.el --- Set up Org file

;; use https://code.tecosaur.net/tec/org-mode
;; with async LaTeX preview support
;;
;; installation
;; 1. git clone https://code.tecosaur.net/tec/org-mode.git && cd org-mode
;; 2. make

(require 'org)
(require 'valign)

;; babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp       . t)
   (maxima     . t)
   (dot        . t)
   (C          . t)
   (python     . t)
   (ruby       . t)
   (shell      . t)
   (gnuplot    . t)))

;; display image after org babel eval

(add-hook 'org-babel-after-execute-hook #'org-display-inline-images)

;; Org image preview

(setq org-image-actual-width nil)

;; Org Table prettify

(add-hook 'org-mode-hook #'valign-mode)

;; Org LaTeX support

(require 'init-latex)

;; CDLaTeX in org-mode

(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

;; 中文支持 XeLaTeX

(setq org-latex-compiler "xelatex")

;; xelatex does not support precompile

(setq org-latex-precompile nil)
(setq org-latex-preview-process-precompiled nil)

;; LaTeX xeCJK 中文字符

(add-to-list 'org-latex-packages-alist
             '("" "xeCJK"    t ("xelatex")))

;; amsmath and amsfonts for \mathcal \mathbb like

(add-to-list 'org-latex-packages-alist
             '("" "amsmath"  t ("xelatex")))
(add-to-list 'org-latex-packages-alist
             '("" "amsfonts" t ("xelatex")))
(add-to-list 'org-latex-packages-alist
             '("" "amssymb" t ("xelatex")))

;; Org code block to LaTeX code environment
;; use listings for fast compile and no _minted folder

(setq org-latex-listings 'listings)
(setq org-latex-custom-lang-environments
      '((python "pythoncode")
        (lisp   "commonlispcode")
        (elisp  "common-lispcode")))

;; overwrite the default listing options

(setf org-latex-listings-options
      '(("showspaces" "false")
        ("breaklines" "true")
        ("keepspaces" "true")
        ("showstringspaces" "false")
        ("basicstyle" "\\ttfamily")
        ("frame" "single")
        ("captionpos" "b")
        ("extendedchars" "true")
        ("frame" "lines")
        ("numbers" "left")
        ("numberstyle" "\\tiny")))

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;; LaTeX preview support

(plist-put
 (cdr (assoc 'dvisvgm org-latex-preview-process-alist))
 :image-converter
 '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -v4 -o %B-%%9p.svg %f"))

;; LaTeX equation input prettify

(add-hook 'org-mode-hook #'ryo.ui:setup-latex-prettify-symbol-mode)

(setq org-pretty-entities-include-sub-superscripts nil)

(require 'org-appear)

(setq org-hide-emphasis-markers t)
(setq org-appear-autoemphasis   t)
(setq org-pretty-entities       t)
(setq org-appear-autoentities   t)
(setq org-hidden-keywords       nil)
(setq org-appear-autokeywords   t)
(setq org-appear-inside-latex   nil)
(setq org-appear-delay          0)
(setq org-appear-trigger        'always)

(add-hook 'org-mode-hook #'org-appear-mode)

;; org-ref
;; https://github.com/jkitchin/org-ref/

(require 'org-ref)
(require 'doi-utils)

;; (define-key org-mode-map (kbd "C-c ]") #'

(setq org-latex-pdf-process
      (list "latexmk -shell-escape -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

(setq bibtex-completion-notes-template-multiple-files
      "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-display-formats
      '((article
         . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
        (inbook
         . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
        (incollection
         . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
        (inproceedings
         . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
        (t
         . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
      bibtex-completion-pdf-open-function
      (lambda (fpath) (call-process "open" nil 0 nil fpath)))

(provide 'init-org)

;;; init-org.el ends here
