;;; init-python.el --- Setup Python for Emacs

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

(provide 'init-python)

;;; init-python.el ends here
