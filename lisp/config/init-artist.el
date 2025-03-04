;;; init-artist.el --- Artist mode made it easy to use

(require 'artist)

;; quit artist mode
(define-key artist-mode-map (kbd "q") #'artist-mode-off)

;; popup a mouse menu
(define-key artist-mode-map (kbd "a") #'artist-mouse-choose-operation)

(provide 'init-artist)

;;; init-artist.el ends here
