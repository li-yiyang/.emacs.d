;;; init-web.el --- Set up web viewing for Emacs

;; url shall be stored as privates infomations

(setq url-configuration-directory
      (expand-file-name "privates/url" user-emacs-directory))

(provide 'init-web)

;;; init-web.el ends here
