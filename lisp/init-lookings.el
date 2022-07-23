;;; init-lookings.el --- Emacs Lookings -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://www.linw1995.com/en/blog/Using-Emacs-Editor-For-The-First-Time/

;;; Code:

;; Dashboard: https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
             :config
             (dashboard-setup-startup-hook)
             (setq dashboard-center-content t))

;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-dark t))

(provide 'init-lookings)
;;; init-lookings.el ends here
