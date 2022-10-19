;;; init-markdown.el --- Markdown -*- lexical-binding: t -*-
;;; Commentary:

;; refering: https://jblevins.org/projects/markdown-mode/

;;; Code:
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")
	      markdown-enable-math t
	      markdown-list-indent-width 2))

;; Markdown-TOC
(use-package markdown-toc)

(provide 'init-markdown)
;;; init-markdown.el ends here
