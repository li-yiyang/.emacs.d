;;; init-paredit.el --- Paredit -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://wikemacs.org/wiki/Paredit-mode

;;; Code:
(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  (add-hook 'racket-mode-hook 'enable-paredit-mode))

(provide 'init-paredit)
;;; init-paredit.el ends here
