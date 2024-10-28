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

(dirvish-override-dired-mode)

(defgroup ryo.dired ()
  "Ryo's configures for dired. "
  :prefix "ryo.dired:")

(cl-defmacro extcase (file-name &body cases)
  "Case by `file-name' extension.
Return `t' if matches, otherwise `nil'.

Syntax:
 * `file-name': expr for file-name
 * `cases': case should be like (ext-type . progn)
   + `ext-type' can be
     + string for single ext pattern
     + list of string for multiple ext pattern
     + `:otherwise' (should be the last case)
       for otherwise
"
  (let ((ext-name (gensym "EXT-NAME")))
    `(let ((,ext-name (file-name-extension ,file-name)))
       (cond
        ,@(cl-loop for (ext-type* . progn) in cases
                   if (listp ext-type*)
                   collect `((or ,@(mapcar (lambda (ext-type)
                                             `(string= ,ext-name ,ext-type))
                                           ext-type*))
                             ,@progn
                             t)
                   else if (stringp ext-type*)
                   collect `((string= ,ext-name ,ext-type*) ,@progn)
                   else if (eq ext-type* :otherwise)
                   collect `(t ,@progn nil)
                   while (not (eq ext-type* :otherwise)))))))

;; tar xzf tar.gz

(defun ryo.dired:dired-uncompress-file ()
  "In Dired, uncompress this file (if it is a tar file) right here. "
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (when (extcase file-name
                   (("gz" "tar")
                    (message (concat "Untar " file-name " ..."))
                    (shell-command (concat "tar xzf " file-name)))
                   ("zip"
                    (message (concat "Unzip " file-name " ..."))
                    (shell-command (concat "unzip " file-name)))
                   (:otherwise
                    (message "Not a known compressed file")))
      (message (concat "Finish uncompress " file-name))
      (revert-buffer-quick))))

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
(define-key dirvish-mode-map (kbd "RET") #'dired-find-file-other-window)
(define-key dirvish-mode-map (kbd "X")   #'ryo.dired:dired-uncompress-file)

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

(provide 'init-dired)

;;; init-dired.el ends here
