;;; init-latex.el --- Latex -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://jacklovel.github.io/2020/10/12/emacs+latex配置.html

;;; Code:
;; sure, using auctex, but some tricky naming problems:
;; https://emacs.stackexchange.com/questions/41321/when-to-specify-a-package-name-in-use-packages-ensure-tag/41324#41324
(use-package tex
  :ensure auctex
  :config
  ; (setq TeX-auto-save t)
  (setq-default TeX-master nil)
  (setq TeX-parse-self t))

;; xelatex compile
(add-hook 'LaTeX-mode-hook
  (lambda()
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX"
	TeX-save-query nil
	TeX-show-compilation nil
	TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
))

;; magic-latex-buffer
;; repo: https://github.com/zk-phi/magic-latex-buffer
(use-package magic-latex-buffer
 :config
 (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
 (setq magic-latex-enable-block-highlight t
       magic-latex-enable-suscript        t
       magic-latex-enable-pretty-symbols  t
       magic-latex-enable-block-align     nil
       magic-latex-enable-inline-image    t
       magic-latex-enable-minibuffer-echo t
       ))

;; Prettify Math (too slow...)
;; https://github.com/shaqxu/prettify-math
(use-package prettify-math
  ;; :host nil
  ;; :repo "https://gitee.com/shaqxu/prettify-math"
  ;; :files (:defaults "mathjax-jsonrpc.js")
  :config
  (setq prettify-math-delimiters-alist
  '(("$" tex)
    ("$$" tex block)
    (("\\(" . "\\)") tex block)
    ("`" asciimath)
    ("``" asciimath block)))
  (setq prettify-math-default-scale 1.2)
  ; (add-hook 'latex-mode-hook 'prettify-math-mode)
  ; (add-hook 'markdown-mode-hook 'prettify-math-mode)
  )

;; Math preview
;; https://gitlab.com/matsievskiysv/math-preview
;(use-package math-preview
;  :custom
;  (math-preview-command "/opt/homebrew/bin/math-preview"))

;; Latex Math complete for company
;; https://github.com/vspinu/company-math
(use-package company-math
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  )

;; Latex company auctex
;; https://github.com/alexeyr/company-auctex/
(use-package company-auctex
  :config
  (company-auctex-init))

(provide 'init-latex)
;;; init-latex.el ends here
