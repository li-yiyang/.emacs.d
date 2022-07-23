;;; init-proj-man.el --- Project Manager -*- lexical-binding: t -*-
;;; Commentary:

;; repo: https://github.com/bbatsov/projectile

;;; Code:

(use-package projectile
             :config
             (setq projectile-cache-file (expand-file-name ".cache/projectile.cache" user-emacs-directory))
             (projectile-mode 1)
             (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

(use-package helm-projectile
             :if (function 'helm)
             :config
             (helm-projectile-on))

(provide 'init-proj-man)
;;; init-proj-man.el ends here
