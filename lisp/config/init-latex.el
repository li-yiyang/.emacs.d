;;; init-latex.el --- Set up for LaTeX -*- lexical-binding: t -*-

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
    ("\\hookrightarrow" . ?↪)
    ("\\hookleftarrow" . ?↩)

    ;; \xrightarrow and \xleftarrow should differ from
    ;; normal \rightarrow and \leftarrow
    ;;
    ;; unicode from http://xahlee.info/comp/unicode_math_operators.html
    ("\\xrightarrow" . ?⥅)
    ("\\xleftarrow" . ?⥆)
    ("\\underbrace" . ?⎵)

    ("\\frac"   . ?𝐟)
    ("\\sqrt"   . ?√)
    ("\\updownarrow" . ?↕)
    ("\\boldsymbol" . ?𝐛)
    ("\\mathbb" . ?𝐁)
    ("\\mathrm" . ?𝐫)
    ("\\mathcal" . ?𝐜)
    ("\\Vert" . ?‖)
    ("\\Vert" . ?‖)
    ("\\left\\Vert" . ?‖)
    ("\\right\\Vert" . ?‖)
    ("\\left\\lfloor" . ?⌊)
    ("\\right\\rfloor" . ?⌋)
    ("\\left\\lceil"  . ?⌈)
    ("\\right\\rceil" . ?⌉)
    ("\\left\\langle" . ?⟨ )
    ("\\right\\rangle" . ?⟩ )))

(defun ryo.ui:setup-latex-prettify-symbol-mode ()
  "Set up LaTeX prettify symbol. "
  (interactive)
  (setq-local prettify-symbols-alist
              (append prettify-symbols-alist
                      ryo.ui:latex-prettify-symbols-alist))
  (prettify-symbols-mode t))

;; overwrite default \longleftarrow and \longrightarrow
;; to \xleftarrow and \xrightarrow (need amsmath package)

(add-to-list 'cdlatex-math-symbol-alist
             '(?< ("\\leftarrow" "\\xleftarrow" "\\min")))
(add-to-list 'cdlatex-math-symbol-alist
             '(?> ("\\rightarrow" "\\xrightarrow" "\\max")))
(add-to-list 'cdlatex-math-symbol-alist
             '(?* ("\\times" "\\circ" "\\otimes")))

;; load for AUCTeX

(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook #'ryo.ui:setup-latex-prettify-symbol-mode)

;; ebib

(require 'ebib)

(global-set-key (kbd "C-c e") #'ebib)

(provide 'init-latex)

;;; init-latex.el ends here
