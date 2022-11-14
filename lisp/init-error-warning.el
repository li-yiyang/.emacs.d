;;; init-error-warning.el --- Errors and Warning Engine -*- lexical-binding: t -*-
;;; Commentary:

;; repo: https://www.flycheck.org/en/latest/
;; referring: https://nyk.ma/posts/emacs-write-your-own/#flycheck

;;; Code:

(use-package flycheck
             :init
             (setq flycheck-emacs-lisp-load-path 'inherit)
             :config
             (global-flycheck-mode)
	     (setq flycheck-emacs-lisp-load-path 'inherit))

; (use-package )

(provide 'init-error-warning)
;;; init-error-warning.el ends here
