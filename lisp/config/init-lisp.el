;;; init-lisp.el --- Set up for Common Lisp -*- lexical-binding: t; -*-

(require 'sly)
(require 'sly-autoloads)
(require 'acm)

(defgroup ryo.lisp ()
  "Ryo's configures for Common Lisp. "
  :prefix "ryo.lisp:")

(defcustom ryo.lisp:sly-port 4005
  "The default SLY mrepl connect port (\\=`ryo.lisp:sly-port\\=')
used in \\=`ryo.lisp:start-or-connect-to-lisp\\=' function."
  :group 'ryo.lisp
  :type  'integer)

(defcustom ryo.lisp:sbcl-dynamic-space-size 4096
  "The default SBCL start dynamic space size.
See \\=`ryo.lisp:make-inferior-lisp-program\\='. "
  :group 'ryo.lisp
  :type  'integer)

(defcustom ryo.lisp:start-with-sly-connected-p nil
  "Whether start with a SLY connected.
If non-nil, will call \\=`ryo.lisp:start-or-connect-to-lisp\\='. "
  :group 'ryo.lisp)

(defun ryo.lisp:make-inferior-lisp-program ()
  "Make a inferior lisp program list for
\\=`inferior-lisp-program\\='. "
  (list "sbcl" "--dynamic-space-size"
        (format "%d" ryo.lisp:sbcl-dynamic-space-size)
        "--control-stack-size" "24"))

;; org-mode babel

(setq org-babel-lisp-eval-fn #'sly-eval)

;; sbcl with larger dynamic space size

(setq inferior-lisp-program (ryo.lisp:make-inferior-lisp-program))

(defun ryo.lisp:smart-sly-mrepl-return ()
  "Run `sly-mrepl-return' according to cursor position.
If at the end of buffer, call `sly-mrepl-return';
otherwise, call `newline'. "
  (interactive)
  ;; (require 'sly-mrepl)
  (if (eq (point) (point-max))
      (sly-mrepl-return)
    (newline)))

(defun ryo.lisp:regist-sly-mrepl-key-map ()
  "Setup SLY mrepl mode key binding. "
  (require 'sly-mrepl)
  (define-key sly-mrepl-mode-map (kbd "RET")        #'ryo.lisp:smart-sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "C-<return>") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "S-<return>") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "M-h v")      #'sly-describe-symbol)
  (define-key sly-mrepl-mode-map (kbd "M-h f")      #'sly-describe-function)
  (define-key sly-mrepl-mode-map (kbd "M-h c")      #'sly-who-calls)
  (define-key sly-mrepl-mode-map (kbd "M-h b")      #'sly-who-binds)
  (define-key sly-mrepl-mode-map (kbd "M-h h")      #'sly-documentation-lookup))

(add-hook 'sly-mrepl-mode-hook #'ryo.lisp:regist-sly-mrepl-key-map)

;; init with sly-mrepl connected

(defun ryo.lisp:start-or-connect-to-lisp ()
  "Connect to a SLY server.

This will only be avaliable if \\=`ryo.lisp:start-with-sly-connected-p\\='
is non-nil. 

If fails, try to open a daemon SBCL using screen if continued.
The started lisp program will be use \\=`ryo.lisp:make-inferior-lisp-program\\='
to create the starting env. "
  (condition-case nil
      (when ryo.lisp:start-with-sly-connected-p
        (sly-connect "localhost" ryo.lisp:sly-port))
    (error
     (when (string= (completing-read "SLY not founded, create? [Y/n]"
                                     '("y" "n")
                                     nil t "y")
                    "y")
       (shell-command
        (concat "screen -dmS sbcl "
                (string-join (ryo.lisp:make-inferior-lisp-program) " ")
                "--eval \"(ql:quickload :slynk)\" "
                "--eval \"(slynk:create-server :dont-close t :port "
                ryo.lisp:sly-port
                ")\""))
       (run-with-idle-timer
        1 nil #'(lambda () (sly-connect "localhost" ryo.lisp:sly-port)))))))

(add-hook 'emacs-startup-hook #'ryo.lisp:start-or-connect-to-lisp)

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

(defun acm-backend-sly-candidates (keyword)
  (mapcar
   (lambda (candidate)
     (let* ((sly-type (get-text-property 0 'sly--classification candidate))
	    ;; use first part as icon
	    (acm-icon (pcase (car (split-string sly-type ","))
			("fn"             "function")
			("generic-fn"     "method")
			("cla"            "class")
			("special-op"     "operator")
			("type"           "class")
			("constant"       "constant")
			("var"            "variable")
			("pak"            "package")
			("macro"          "macro")
			(_                "unknown"))))
       (list :key          candidate
	     :icon         acm-icon
	     :label        candidate
	     :displayLabel candidate
	     :annotation   sly-type
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
	;; use slynk-apropos::briefly-describe-symbol-for-emacs
	;; for variable (make sure it won't print too much of its value).
	((or "variable")
	 (sly-eval
	    `(slynk::with-buffer-syntax
	      ()
	      (cl:format cl:nil "~A"
			 (cl:getf (slynk-apropos::briefly-describe-symbol-for-emacs
				   (slynk::parse-symbol-or-lose ,key) cl:nil)
				  :variable)))))
	(_
	 (sly-eval `(slynk:describe-symbol ,key)))))))

(defun acm-update-candidates-append-sly-results (fn)
  (if (and (or (derived-mode-p 'lisp-mode)
	       (derived-mode-p 'sly-mrepl-mode))
	   (sly-connected-p))
      (let* ((keyword (acm-get-input-prefix))
	     (sly-res (acm-backend-sly-candidates keyword)))
	(append sly-res (funcall fn)))
    (funcall fn)))
(advice-add 'acm-update-candidates
	    :around #'acm-update-candidates-append-sly-results)

(add-hook 'lisp-mode-hook      #'lsp-bridge-mode)
(add-hook 'sly-mrepl-mode-hook #'lsp-bridge-mode)

(add-hook 'lisp-mode-hook      #'hs-minor-mode)
(define-key lisp-mode-map (kbd "C-c C-f") #'hs-toggle-hiding)

(provide 'init-lisp)

;;; init-lisp.el ends here
