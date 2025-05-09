;;; init-vertico.el --- Vertico in Minibuffer -*- lexical-binding: t; -*-

(require 'vertico)
(require 'consult)
(require 'marginalia)
(require 'vertico-posframe)

(vertico-mode)
(vertico-posframe-mode 1)

;; Use unicode instead of ASCII for vertico-posframe
(unless (display-graphic-p)
  (standard-display-unicode-special-glyphs))

;; Consult
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s i") #'isearch-forward)
(global-set-key (kbd "C-s l") #'consult-line)
(global-set-key (kbd "C-s g") #'consult-grep)

(setq xref-show-xrefs-function       #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Marginalia

(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)
(marginalia-mode)

(provide 'init-vertico)

;;; init-vertico.el ends here
