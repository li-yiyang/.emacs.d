;;; init-c.el --- C Language Support -* lexical-binding: t -*-
;;; Commentary:

;; referring: http://blog.binchen.org/posts/emacs-as-c-ide-easy-way.html

;;; Code:

(use-package company-gtags)
(use-package emacs-helm-gtags)

;; Autocomplete for company
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; company-irony
;; https://github.com/Sarcasm/company-irony
(use-package company-irony
  :config
  (add-to-list 'company-backends 'company-irony))


(provide 'init-c)
;;; init-c.el ends here
