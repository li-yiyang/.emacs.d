;;; init-dired.el --- Setup for dired

;; dirvish --- replace dired
;; see https://github.com/alexluigit/dirvish
;;
;; requirement
;; + brew install coreutils fd poppler ffmpegthumbnailer mediainfo imagemagick

(require 'dirvish)
(require 'dirvish-fd)
(require 'dirvish-ls)
(require 'dirvish-quick-access)
(require 'dirvish-extras)
(require 'dirvish-yank)
(require 'dirvish-history)

(setq delete-by-moving-to-trash t)
(setq dirvish-use-mode-line nil)

;; store dirvish cache to privates

(setq dirvish-cache-dir
      (expand-file-name "privates/dirvish/" user-emacs-directory))

;; Following Config is copied from dirvish sample config
;; https://github.com/alexluigit/dirvish/blob/119f9f59a618bb7b476c93e9ab1d7542c5c1df41/docs/CUSTOMIZING.org#sample-config

;; dirvish attribution

(setq dirvish-attributes '(collapse subtree-state vc-state git-msg))

(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")

;; use dirvish-fd to find file

(bind-key (kbd "C-c f") #'dirvish-fd)

(define-key dirvish-mode-map (kbd "a")   #'dirvish-quick-access)
(define-key dirvish-mode-map (kbd "y")   #'dirvish-yank-menu)
(define-key dirvish-mode-map (kbd "N")   #'dirvish-fd)
(define-key dirvish-mode-map (kbd "s")   #'dirvish-quicksort)
(define-key dirvish-mode-map (kbd "f")   #'dirvish-file-info-menu)
(define-key dirvish-mode-map (kbd "v")   #'dirvish-vc-menu)
(define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
(define-key dirvish-mode-map (kbd "M-t") #'dirvish-layout-toggle)
(define-key dirvish-mode-map (kbd "M-m") #'dirvish-mark-menu)
(define-key dirvish-mode-map (kbd "b")   #'dirvish-history-go-backward)
(define-key dirvish-mode-map (kbd "^")   #'dired-up-directory)

;; mouse support

(setq dired-mouse-drag-files t)
(setq mouse-drag-and-drop-region-cross-program t)

(setq mouse-1-click-follows-link nil)
(define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
(define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)

;; macos ls fix
;; see https://github.com/alexluigit/dirvish/blob/119f9f59a618bb7b476c93e9ab1d7542c5c1df41/docs/CUSTOMIZING.org#listing-directory-failed-but-access-file-worked-error-on-macos

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

(dirvish-override-dired-mode)

(provide 'init-dired)

;;; init-dired.el ends here
