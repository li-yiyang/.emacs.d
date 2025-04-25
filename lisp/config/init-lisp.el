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

(defcustom ryo.lisp:sbcl-dynamic-space-size 20480
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
        (sly-connect "localhost" (format "%d" ryo.lisp:sly-port)))
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
(define-key lisp-mode-map (kbd "M-h d") #'sly-edit-definition)
(define-key lisp-mode-map (kbd "M-h c") #'sly-who-calls)
(define-key lisp-mode-map (kbd "M-h b") #'sly-who-binds)
(define-key lisp-mode-map (kbd "M-h s") #'sly-who-sets)
(define-key lisp-mode-map (kbd "M-h p") #'sly-who-specializes)
(define-key lisp-mode-map (kbd "M-h h") #'sly-documentation-lookup)
(define-key lisp-mode-map (kbd "M-h l") #'sly-hyperspec-lookup)


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

;; hypersec-lookup--hyperspec-lookup-eww
;; see: http://dnaeon.github.io/common-lisp-hyperspec-lookup-using-w3m/

;; should set common-lisp-hyperspec-root to /path/to/your/local/HyperSpec
(defun hyperspec-lookup--hyperspec-lookup-eww (orig-fun &rest args)
  (let ((browse-url-browser-function 'eww-open-file))
    (apply orig-fun args)))
(advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-eww)

;; sly-db-insert-condition wrapper
;; make it better for reading

(cl-defun ryo:wrap-string-into-lines (string &optional (length 70))
  "Wrap long string into shorter lines. "
  (with-temp-buffer
    (let ((fill-column length))
      (insert string)
      (fill-region (point-min) (point-max))
      (buffer-string))))

(defun sly-db-insert-condition--wrap-into-lines (orig-fun &rest args)
  (cl-destructuring-bind (msg type extras) (first args)
    (funcall orig-fun (list (ryo:wrap-string-into-lines msg) type extras))))
(advice-add 'sly-db-insert-condition :around
            #'sly-db-insert-condition--wrap-into-lines)

(defun ryo.lisp:fold-all ()
  "Fold All Lisp lists. "
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (cl-loop for point = (forward-list -1)
             while (/= point 1)
             do (hs-hide-block)
             do (move-beginning-of-line 1))))

;; Org babel

(setq org-babel-lisp-dir-fmt "(uiop:with-current-directory (#P%S)\n %%s\n)")

(defun org-babel-expand-body:lisp (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel--get-vars params))
         (result-params (cdr (assq :result-params params)))
         (print-level nil) (print-length nil)
         (prologue (cdr (assq :prologue params)))
         (epilogue (cdr (assq :epilogue params)))
         (body (if (null vars) (org-trim body)
                 (concat "(let ("
                         (mapconcat
                          (lambda (var)
                            (format "(%S (quote %S))" (car var) (cdr var)))
                          vars "\n      ")
                         ")\n"

                         ;; Make lisp code variable ignorable
                         "  (declare (ignorable "
                         (mapconcat (lambda (var) (format "%S" (car var))) vars " ")
                         "))\n"

                         (and prologue (concat prologue "\n"))
                         body
                         (and epilogue (concat "\n" epilogue "\n"))
                         ")"))))
    (if (or (member "code" result-params)
            (member "pp" result-params))
        (format "(pprint %s)" body)
      body)))

(provide 'init-lisp)

;;; init-lisp.el ends here
