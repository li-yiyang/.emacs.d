;;; init-maxima.el --- Maxima interface

(require 'maxima)

(add-to-list 'auto-mode-alist        '("\\.mac" . maxima-mode))
(add-to-list 'interpreter-mode-alist '("maxima" . maxima-mode))

(provide 'init-maxima)

;;; init-maxima.el ends here
