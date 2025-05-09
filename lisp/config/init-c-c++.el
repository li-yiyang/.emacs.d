;;; init-c-c++.el --- Set up for C and C++ like language. -*- lexical-binding: t -*-

(require 'cc-mode)
(require 'init-lsp)

;; LSP

;; hs-minor-mode
(define-key c-mode-map   (kbd "C-c C-f") #'hs-toggle-hiding)
(define-key c++-mode-map (kbd "C-c C-f") #'hs-toggle-hiding)

(add-hook 'c-mode-hook   #'hs-minor-mode)
(add-hook 'c++-mode-hook #'hs-minor-mode)

(provide 'init-c-c++)

;;; init-c-c++.el ends here
