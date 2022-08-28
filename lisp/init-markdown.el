;;; init-markdown.el --- Markdown -*- lexical-binding: t -*-
;;; Commentary:

;; refering: https://jblevins.org/projects/markdown-mode/

;;; Code:
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(provide 'init-markdown)
;;; init-markdown.el ends here
