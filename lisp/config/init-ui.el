;;; init-ui.el --- Set up Emacs UI

;; This is the basic GUI settings for Emacs.

(defgroup ryo.ui nil
  "Ryo's emacs's ui group."
  :prefix "ryo.ui:")

;; GUI Emacs
(when (display-graphic-p)
  ;; turn off scroll bar
  (scroll-bar-mode -1)

  ;; emacs-plus patches for mac

  (when (eq system-type 'darwin) 		; for mac
    (require 'init-mac-ui)))

;; TUI Emacs
(unless (display-graphic-p)
  (require 'init-tui))

(provide 'init-ui)

;;; init-ui.el ends here
