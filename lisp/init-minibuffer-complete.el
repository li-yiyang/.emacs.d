;;; init-minibuffer-complete.el --- Minibuffer Complete -*- lexical-binding: t -*-
;;; Commentary:

;; Using helm as Minibuffer Complete
;; repo: https://github.com/emacs-helm/helm
;; config referring: https://nyk.ma/posts/emacs-write-your-own/#helm

;;; Code:

;; changed for ivy
;; https://github.com/abo-abo/swiper
;; https://oremacs.com/swiper/
(use-package counsel)
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;(use-package helm
;             :bind (("M-x" . helm-M-x)
;                    ("C-x C-f" . helm-find-files))
;             :config
;             ;; enable helm minor mode globaly
;             (helm-mode 1))

(provide 'init-minibuffer-complete)
;;; init-minibuffer-complete.el ends here
