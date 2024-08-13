;;; init-c-c++.el --- Set up for C and C++ like language.

(require 'init-lsp)

(define-key c-mode-map (kbd "M-h r") #'lsp-bridge-find-references)
(define-key c-mode-map (kbd "M-h d") #'lsp-bridge-find-def)
(define-key c-mode-map (kbd "M-h f") #'lsp-bridge-show-documentation)
(define-key c-mode-map (kbd "M-h i") #'lsp-bridge-find-impl)

(define-key c++-mode-map (kbd "M-h r") #'lsp-bridge-find-references)
(define-key c++-mode-map (kbd "M-h d") #'lsp-bridge-find-def)
(define-key c++-mode-map (kbd "M-h f") #'lsp-bridge-show-documentation)
(define-key c++-mode-map (kbd "M-h i") #'lsp-bridge-find-impl)

(provide 'init-c-c++)

;;; init-c-c++.el ends here
