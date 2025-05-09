;;; init-lsp.el --- Set up Language Server Protocol. -*- lexical-binding: t -*-

;; This file setup the LSP.
;; Use Eglot + lsp-boosted + Corfu
;; Eglot came built-in after Emacs 29

;; for lsp-bridge, see init-lsp-bridge.el

(require 'eglot)
(require 'corfu)
(require 'orderless)
(require 'init-yas)

;; Corfu completion
(setq corfu-cycle                      t
      corfu-quit-at-boundary           nil
      corfu-quit-no-match              t
      corfu-preview-current            nil
      corfu-auto                       t
      corfu-auto-delay                 0.1
      corfu-aotu-prefix                0.1

      text-mode-ispell-word-completion nil
      tab-always-indent                'complete
      read-extended-command-predicate  #'command-completion-default-include-p)

(global-corfu-mode)

;; Orderless

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; Eglot

(define-key eglot-mode-map (kbd "M-h d") #'xref-find-definitions)
(define-key eglot-mode-map (kbd "M-h v") #'eldoc)
(define-key eglot-mode-map (kbd "M-h a") #'eglot-code-actions)

(provide 'init-lsp)

;;; init-lsp.el ends here
