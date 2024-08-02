;;; init-git.el --- Set up for git

;; Magit
;;
;; requirement:
;; + compat
;;   https://github.com/emacs-compat/compat
;; + dash
;;   https://github.com/magnars/dash.el
;; + with-editor
;;   https://github.com/magit/with-editor
;; + transient
;;   https://github.com/magit/transient

(require 'magit)
(require 'transient)

;; transient saved under privates

(setq transient-history-file
      (expand-file-name "privates/transient/history.el" user-emacs-directory))

(provide 'init-git)

;;; init-git.el ends here
