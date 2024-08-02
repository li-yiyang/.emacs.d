;;; init-autosave.el --- Setting up autosave

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

(provide 'init-autosave)

;;; init-autosave.el ends here
