;;; init-minibuffer-complete.el --- Minibuffer Complete -*- lexical-binding: t -*-
;;; Commentary:

;; Using helm as Minibuffer Complete
;; repo: https://github.com/emacs-helm/helm
;; config referring: https://nyk.ma/posts/emacs-write-your-own/#helm

;;; Code:

(use-package helm
             :bind (("M-x" . helm-M-x)
                    ("C-x C-f" . helm-find-files))
             :config
             ;; enable helm minor mode globaly
             (helm-mode 1))

(provide 'init-minibuffer-complete)
;;; init-minibuffer-complete.el ends here
