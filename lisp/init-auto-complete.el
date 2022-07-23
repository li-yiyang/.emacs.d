;;; init-auto-complete.el --- Code Auto Complete -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://nyk.ma/posts/emacs-write-your-own/#文本补全引擎
;; repo: https://company-mode.github.io

;;; Code:

(use-package company
             :hook (after-init . global-company-mode)
             :config
             (setq company-tooltip-align-annotations t ; comment right alian
                   company-tooltip-limit 20 ; optional choices number in menu
                   company-show-numbers t ; M-<number> quick jump
                   company-idle-delay .2 ; delay seconds to pop out
                   company-minimum-prefix-length 1 ; complete after <n> words
                   ))

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
