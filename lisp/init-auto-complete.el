;;; init-auto-complete.el --- Code Auto Complete -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://nyk.ma/posts/emacs-write-your-own/#文本补全引擎
;; repo: https://company-mode.github.io

;;; Code:

(use-package company
  :hook (after-init . global-company-mode)  :config
  (setq company-tooltip-align-annotations t ; comment right alian
        company-tooltip-limit 25     ; optional choices number in menu
        company-show-numbers t	     ; M-<number> quick jump
        company-idle-delay .3	     ; delay seconds to pop out
        company-minimum-prefix-length 2 ; complete after <n> words
        ))

;(use-package flycheck
;  :init
;  (setq flycheck-emacs-lisp-load-path 'inherit)
;  :config
;  (global-flycheck-mode))

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
