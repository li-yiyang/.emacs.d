;;; init-autosave.el --- Setting up autosave -*- lexical-binding: t -*-

;; see https://github.com/manateelazycat/auto-save

(require 'auto-save)
(auto-save-enable)

;; quietly save

(setq auto-save-silent t)

;; automatically delete spaces at the end of the line when saving

(setq auto-save-delete-trailing-whitespace t)

;; vundo --- Visualize the undo tree.
;; see https://github.com/casouri/vundo

(require 'vundo)

(setq vundo-glyph-alist vundo-unicode-symbols)

;; auto-sudoedit
;; see https://github.com/ncaq/auto-sudoedit

(require 'auto-sudoedit)

;; Just hook on `find-file-hook', don't hook `dired-mode-hook', it's unnecessary.
;; see https://github.com/manateelazycat/lazycat-emacs/blob/535b5527b495abb3cfd2bf03e5d242e5eddf8d47/site-lisp/config/init-auto-sudoedit.el#L84C1-L88C50

(add-hook 'find-file-hook #'auto-sudoedit)

(provide 'init-autosave)

;;; init-autosave.el ends here
