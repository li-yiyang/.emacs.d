;;; init-latex.el --- Set up for LaTeX

;; AUCTeX --- TeX support
;; requirement
;; + TeX system
;;   e.g. basictex
;;        brew install basictex   # for TeX system
;;        tlmgr install <package> # for package install
;; + ghostscript
;;   brew install ghostscript
;;
;; installation
;; 1. git clone https://git.savannah.gnu.org/git/auctex.git && cd auctex
;; 2. make
;;    described in README.GIT, to generate missing files
;;    see https://www.gnu.org/software/auctex/manual/auctex/Configure.html
;;
;; see https://www.gnu.org/software/auctex/

(require 'tex)

;; CDLaTeX --- for fast LaTeX input
;; see https://github.com/cdominik/cdlatex/

(require 'cdlatex)

;; some simple math modify alist

(setq cdlatex-math-modify-alist
      '((?b "\\boldsymbol" "\\textbf" t nil nil)
	(?B "\\mathbb"     "\\textbf" t nil nil)))

(defvar ryo.ui:latex-prettify-symbols-alist
  '(("\\begin"  . ?▼)
    ("\\end"    . ?▲)
    ("\\mapsto" . ?↦)
    ("\\multimap" . ?⊸)
    ("\\frac"   . ?𝐟)
    ("\\sqrt"   . ?√)
    ("\\updownarrow" . ?↕)
    ("\\boldsymbol" . ?𝐛)
    ("\\mathbb" . ?𝐁)
    ("\\mathrm" . ?𝐫)
    ("\\mathcal" . ?𝐜)
    ("\\left\\Vert" . ?‖)
    ("\\right\\Vert" . ?‖)))

(defun ryo.ui:setup-latex-prettify-symbol-mode ()
  "Set up LaTeX prettify symbol. "
  (interactive)
  (setq-local prettify-symbols-alist
	      (append prettify-symbols-alist
		      ryo.ui:latex-prettify-symbols-alist))
  (prettify-symbols-mode t))

;; load for AUCTeX

(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook #'ryo.ui:setup-latex-prettify-symbol-mode)

(provide 'init-latex)

;;; init-latex.el ends here
