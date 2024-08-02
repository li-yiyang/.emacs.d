;;; init-yas.el --- Set up Yasnippet

;; see https://github.com/joaotavora/yasnippet

(require 'yasnippet)

;; private snippet

(add-to-list 'yas-snippet-dirs
	     (expand-file-name "privates/snippets" user-emacs-directory))

;; globally enable yasnippet

(yas-global-mode 1)

(provide 'init-yas)

;;; init-yas.el ends here
