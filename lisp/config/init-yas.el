;;; init-yas.el --- Set up Yasnippet

;; see https://github.com/joaotavora/yasnippet

(require 'yasnippet)

(defgroup ryo.yas ()
  "Ryo's configures for yasnippet. "
  :prefix "ryo.yas:")

;; snippet functions predefined

(defcustom ryo.yas:user-name (user-full-name)
  "The username to be used when expanding yasnippet
about the author and maintainer. "
  :group 'ryo.yas
  :type  'string)

(defcustom ryo.yas:default-project-url "https://li-yiyang.github.io"
  "The default URL when expanding yasnippet about project URL. "
  :group 'ryo.yas
  :type  'string)

(defun ryo.yas:user-full-name ()
  "Return the \\=`ryo.yas:user-name\\='. "
  ryo.yas:user-name)

(defun ryo.yas:project-url ()
  "Return the project URL for snippet insert.
By default it is \\=`ryo.yas:default-project-url\\='. "
  ;; TODO: add more rules for the project url expanding
  ryo.yas:default-project-url)

;; private snippet

(add-to-list 'yas-snippet-dirs
	     (expand-file-name "privates/snippets" user-emacs-directory))

;; globally enable yasnippet

(yas-global-mode 1)

(provide 'init-yas)

;;; init-yas.el ends here
