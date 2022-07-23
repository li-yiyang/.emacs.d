;;; init-yasnippet.el --- Template -*- lexical-binding: t -*-
;;; Commentary:

;; repo: https://github.com/joaotavora/yasnippet
;; referring: https://nyk.ma/posts/emacs-write-your-own/#template

;;; Code:
(use-package yasnippet
  :config
  (yas-global-mode 1)) ;; switch on globaly

(use-package yasnippet-snippets ;; general template lib
  :after (yasnippet))

(use-package auto-yasnippet
  :bind
  (("C-c & w" . aya-create)
   ("C-c & y" . aya-expand))
  :config(setq aya-persist-snippets-dir
	       (concat user-emacs-directory "my/snippets")))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
