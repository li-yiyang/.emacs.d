;;; init-mail.el --- Emacs as mail client

(require 'rmail)
(require 'smtpmail)
(require 'auth-source)

;;; Getting Mail
;; see https://www.emacswiki.org/emacs/GettingMail

;; NOTE: `rmail-preserve-inbox' may be dangerous.
(setq rmail-preserve-inbox     nil
      rmail-mail-new-frame     t
      rmail-movemail-flags     nil ;; '("-r" "--tls")
      rmail-mime-prefer-html   nil
      rmail-file-coding-system t)

;; Sending Mail

(setq message-send-mail-function 'smtpmail-send-it)

(provide 'init-mail)

;;; init-mail.el ends here
