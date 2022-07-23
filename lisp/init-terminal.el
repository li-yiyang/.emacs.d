;;; init-terminal.el --- Terminal in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; repo: https://github.com/akermu/emacs-libvterm

;;; Code:
(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t)) ;; kill buffer when existing terminal
  

(provide 'init-terminal)
;;; init-terminal.el ends here
