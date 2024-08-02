;;; init-blink-search.el --- Set up for blink-search

;; see https://github.com/manateelazycat/blink-search

(require 'blink-search)

;; store database to privates

(let ((private-path (expand-file-name "privates/blink-search/" user-emacs-directory)))
  (setq blink-search-db-path
	(expand-file-name "blink-search.db" private-path))
  (setq blink-search-history-path
	(expand-file-name "history" private-path)))

;; search bind with C-s

(global-set-key (kbd "C-s") 'blink-search)

(provide 'init-blink-search)

;;; init-blink-search.el ends here
