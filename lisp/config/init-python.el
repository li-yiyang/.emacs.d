;;; init-python.el --- Setup Python for Emacs -*- lexical-binding: t -*-

;; code-cells:
;; open, edit Jupyter notebook in Emacs
;; see https://github.com/astoff/code-cells.el

(require 'code-cells)

;; LSP, using ruff-lsp
;; to install:
;;
;;     pip install ruff ruff-lsp

(require 'init-lsp)

(setq lsp-bridge-python-lsp-server "ruff")

;; lsp-bridge keybinding

(define-key python-mode-map (kbd "M-h f") #'lsp-bridge-popup-documentation)
(define-key python-mode-map (kbd "M-h v") #'lsp-bridge-popup-documentation)

(provide 'init-python)

;;; init-python.el ends here
