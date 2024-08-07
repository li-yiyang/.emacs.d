;;; init-ui.el --- Set up Emacs UI

;; This is the basic GUI settings for Emacs.

;; turn of scroll bar

(scroll-bar-mode -1)

(defgroup ryo.ui nil
  "Ryo's emacs's ui group.")

;; emacs-plus patches for mac

(when (eq system-type 'darwin) 		; for mac
  (require 'init-mac-ui))

(provide 'init-ui)

;;; init-ui.el ends here
