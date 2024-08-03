;;; init-org.el --- Set up Org file

;; use https://code.tecosaur.net/tec/org-mode
;; with async LaTeX preview support
;;
;; installation
;; 1. git clone https://code.tecosaur.net/tec/org-mode.git && cd org-mode
;; 2. make

(require 'org)

;; Org LaTeX support

(require 'init-latex)

;; CDLaTeX in org-mode

(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

;; 中文支持 XeLaTeX

(setq org-latex-compiler "xelatex")

;; LaTeX xeCJK 中文字符

(add-to-list 'org-latex-packages-alist
	     '("" "xeCJK"    t ("xelatex")))

;; amsmath and amsfonts for \mathcal \mathbb like

(add-to-list 'org-latex-packages-alist
	     '("" "amsmath"  t ("xelatex")))
(add-to-list 'org-latex-packages-alist
	     '("" "amsfonts" t ("xelatex")))

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

(plist-put (cdr (assoc 'dvisvgm org-latex-preview-process-alist))
           :image-converter
           '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f"))

(provide 'init-org)

;;; init-org.el ends here
