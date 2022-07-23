;;; init-racket.el --- Racket Language Config -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for racket
;; referring: https://github.com/keyz/c311-get-started

;;; Code:
(use-package racket-mode)

(eval-after-load "racket-mode"
  '(define-key racket-mode-map (kbd "M-RET") 'racket-run))

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

(mapc (lambda (pr) (put (car pr) 'racket-indent-function (cdr pr)))
      '((conde . 0)
        (fresh . 1)
        (run . 1)
        (run* . 1)
        (run . 2)))

;; rainbow-delimiters
(use-package rainbow-delimiters)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(provide 'init-racket)
;;; init-racket.el ends here
