;;; init-lsp.el --- LSP Setup -*- lexical-binding: t -*-
;;; Commentary:

;; repo:

;;; Code:
(use-package lsp-mode
  :commands (lsp)
  :hook (((
	   ruby-mode
	   racket-mode
	   ;; ...
	   ) . lsp))
  :init
  (setq lsp-auto-configure t ;; auto configure
	lsp-auto-guess-root t ;; auto guess project root file
	lsp-idle-delay 0.500 ;; refresh server after idle
	lsp-session-file "~/.emacs.d/.cache/lsp-sessions")) ;; config cache file

(provide 'init-lsp)
;;; init-lsp.el ends here
