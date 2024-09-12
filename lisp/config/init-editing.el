;;; init-editing.el --- Init some basic editing perferance.

;; Basic editing

(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Separedit
;; see https://github.com/twlz0ne/separedit.el

(require 'separedit)

(define-key prog-mode-map (kbd "C-c '") #'separedit)
(setq separedit-default-mode 'fundamental-mode)
(setq separedit-preserve-string-indentation nil)
(setq separedit-continue-fill-column nil)

;; Large file editing support:
;; from https://emacs-china.org/t/topic/25811/9

(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; GhostScripts Support
;; atomic-chrome
;; see: https://github.com/alpha22jp/atomic-chrome

(require 'atomic-chrome)
(atomic-chrome-start-server)


(provide 'init-editing)

;;; init-editing.el ends here
