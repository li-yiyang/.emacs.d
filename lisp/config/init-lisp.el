;;; init-lisp.el --- Set up for Common Lisp -*- lexical-binding: t; -*-

(require 'sly)
(require 'sly-autoloads)
(require 'acm)

;; org-mode babel

(setq org-babel-lisp-eval-fn #'sly-eval)

;; sbcl with larger dynamic space size

(setq inferior-lisp-program
      '("sbcl" "--dynamic-space-size" "4096" "--control-stack-size" "24"))

(defun ryo:regist-sly-mrepl-key-map ()
  "Setup SLY mrepl mode key binding. "
  (require 'sly-mrepl)
  (define-key sly-mrepl-mode-map (kbd "RET")        nil)
  (define-key sly-mrepl-mode-map (kbd "C-<return>") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "S-<return>") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "M-h v")   #'sly-describe-symbol)
  (define-key sly-mrepl-mode-map (kbd "M-h f")   #'sly-describe-function)
  (define-key sly-mrepl-mode-map (kbd "M-h c")   #'sly-who-calls)
  (define-key sly-mrepl-mode-map (kbd "M-h b")   #'sly-who-binds)
  (define-key sly-mrepl-mode-map (kbd "M-h h") #'sly-documentation-lookup))

(add-hook 'sly-mrepl-mode-hook #'ryo:regist-sly-mrepl-key-map)

;; some help keys

(define-key lisp-mode-map (kbd "M-h v") #'sly-describe-symbol)
(define-key lisp-mode-map (kbd "M-h f") #'sly-describe-function)
(define-key lisp-mode-map (kbd "M-h c") #'sly-who-calls)
(define-key lisp-mode-map (kbd "M-h b") #'sly-who-binds)
(define-key lisp-mode-map (kbd "M-h h") #'sly-documentation-lookup)

;; use acm as SLY completion front end
;; currently is only for patching

;; from https://github.com/joaotavora/sly/blob/ba40c8f054ec3b7040a6c36a1ef3e9596b936421/lib/sly-completion.el#L323C9-L335C36
;; and see `acm-icon-alist' for the icon mapping
;; from https://github.com/manateelazycat/lsp-bridge/blob/49b5497243873b1bddea09a4a988e3573ed7cc3e/acm/acm-icon.el#L94

(defun acm-backend-sly-candidate-type (candidate)
  (pcase (get-text-property 0 'sly--classification candidate)
    ("fn"             "function")
    ("generic-fn"     "method")
    ("generic-fn,cla" "method")
    ("cla,type"       "class")
    ("cla"            "class")
    ("special-op"     "operator")
    ("type"           "class")
    ("constant"       "constant")
    ("var"            "variable")
    ("pak"            "package")
    ("pak,constant"   "package")
    ("macro"          "macro")
    (_                "unknown")))

(defun acm-backend-sly-candidates (keyword)
  (mapcar (lambda (candidate)
	    (let ((type (acm-backend-sly-candidate-type candidate)))
	      (list :key          candidate
		    :icon         type
		    :label        candidate
		    :displayLabel candidate
		    :annotation   (capitalize candidate)
		    :backend      "sly")))
	  (ignore-errors
	    (car (funcall sly-complete-symbol-function keyword)))))

;; poor man's lisp doc

(defun acm-backend-sly-candidate-doc (candidate)
  (when (sly-connected-p)
    (let ((type (plist-get candidate :icon))
	  (key  (plist-get candidate :key)))
      (pcase type
	((or "function" "method" "operator")
	 (sly-eval `(slynk:describe-function ,key)))
	(_ (sly-eval `(slynk:describe-symbol ,key)))))))

(defun acm-update-candidates-append-sly-results (fn)
  (if (and (derived-mode-p 'lisp-mode)
	   (sly-connected-p))
      (let* ((keyword (acm-get-input-prefix))
	     (sly-res (acm-backend-sly-candidates keyword)))
	(append sly-res (funcall fn)))
    (funcall fn)))
(advice-add 'acm-update-candidates
	    :around #'acm-update-candidates-append-sly-results)

(add-hook 'lisp-mode-hook      #'lsp-bridge-mode)
(add-hook 'sly-mrepl-mode-hook #'lsp-bridge-mode)

(provide 'init-lisp)

;;; init-lisp.el ends here
