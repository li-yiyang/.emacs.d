;;; init-dot.el --- Setup Graphviz for Emacs -*- lexical-binding: t -*-

(require 'init-lsp)
(require 'graphviz-dot-mode)

(setq graphviz-dot-indent-width 2)
(setq graphviz-dot-preview-extension "svg")

(defun dot-png-preview ()
  (interactive)
  (let ((graphviz-dot-preview-extension "png"))
    (graphviz-dot-preview)))

(define-key graphviz-dot-mode-map (kbd "C-c C-p") #'dot-png-preview)

(provide 'init-dot)

;;; init-dot.el ends here
