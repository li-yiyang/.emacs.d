;;; init-markdown.el --- Setup Markdown for Emacs -*- lexical-binding: t -*-

(require 'markdown-mode)
(require 'init-lsp)

;; Use marksman for LSP backend

(setq lsp-bridge-markdown-lsp-server "marksman")

;; hide markdown url

(setq-default markdown-hide-urls t)

;; tab width 2!!!!
;; from http://www.dab.hi-ho.ne.jp/sasa/biboroku/misc/emacs-markdown-mode-tab-width.html

(add-hook 'markdown-mode #'ryo.edit:tab-width-2)

;; some key bindings

(define-key markdown-mode-map (kbd "C-<return>") #'markdown-follow-link-at-point)

(provide 'init-markdown)

;;; init-markdown.el ends here
