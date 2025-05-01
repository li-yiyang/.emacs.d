;;; init-c-c++.el --- Set up for C and C++ like language. -*- lexical-binding: t -*-

(require 'cc-mode)
(require 'init-lsp)

;; LSP
(define-key c-mode-map (kbd "M-h r") #'lsp-bridge-find-references)
(define-key c-mode-map (kbd "M-h d") #'lsp-bridge-find-def)
(define-key c-mode-map (kbd "M-h f") #'lsp-bridge-show-documentation)
(define-key c-mode-map (kbd "M-h i") #'lsp-bridge-find-impl)

(define-key c++-mode-map (kbd "M-h r") #'lsp-bridge-find-references)
(define-key c++-mode-map (kbd "M-h d") #'lsp-bridge-find-def)
(define-key c++-mode-map (kbd "M-h f") #'lsp-bridge-show-documentation)
(define-key c++-mode-map (kbd "M-h i") #'lsp-bridge-find-impl)

;; hs-minor-mode
(define-key c-mode-map   (kbd "C-c C-f") #'hs-toggle-hiding)
(define-key c++-mode-map (kbd "C-c C-f") #'hs-toggle-hiding)

(add-hook 'c-mode-hook   #'hs-minor-mode)
(add-hook 'c++-mode-hook #'hs-minor-mode)

(provide 'init-c-c++)

;;; init-c-c++.el ends here
