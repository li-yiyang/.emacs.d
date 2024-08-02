;;; init-lisp.el --- Set up for Common Lisp

(require 'sly)
(require 'sly-autoloads)

;; org-mode babel

(setq org-babel-lisp-eval-fn #'sly-eval)

;; sbcl with larger dynamic space size

(setq inferior-lisp-program
      '("sbcl" "--dynamic-space-size" "4096" "--control-stack-size" "24"))

(defun ryo:regist-sly-mrepl-key-map ()
  "Setup SLY mrepl mode key binding. "
  (require 'sly-mrepl)
  (define-key sly-mrepl-mode-map (kbd "RET")        nil)
  (define-key sly-mrepl-mode-map (kbd "C-<return>") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "S-<return>") #'sly-mrepl-return))

(add-hook 'sly-mrepl-mode-hook #'ryo:regist-sly-mrepl-key-map)
(add-hook 'sly-mrepl-mode-hook #'electric-pair-local-mode)

;; some help keys
(add-hook 'sly-mrepl-mode-hook #'electric-pair-local-mode)

(define-key lisp-mode-map (kbd "C-l v")   #'sly-describe-symbol)
(define-key lisp-mode-map (kbd "C-l f")   #'sly-describe-function)
(define-key lisp-mode-map (kbd "C-l c")   #'sly-who-calls)
(define-key lisp-mode-map (kbd "C-l b")   #'sly-who-binds)
(define-key lisp-mode-map (kbd "C-l C-l") #'recenter-top-bottom)

(provide 'init-lisp)

;;; init-lisp.el ends here
