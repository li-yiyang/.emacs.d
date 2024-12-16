;;; init-maxima.el --- Maxima interface

(require 'maxima)

(add-to-list 'auto-mode-alist        '("\\.mac" . maxima-mode))
(add-to-list 'interpreter-mode-alist '("maxima" . maxima-mode))

(define-key maxima-mode-map (kbd "M-h")   nil)
(define-key maxima-mode-map (kbd "M-h f") #'maxima-help-at-point)
(define-key maxima-mode-map (kbd "M-h v") #'maxima-help-at-point)

;; Copied from init-lisp.el

(defun maxima-complete ()
  "Complete the current object, depending on context."
  (interactive)
  (let* ((pmin (maxima-form-beginning-position))
         (pps (parse-partial-sexp pmin (point))))
    (cond
     ;; complete filename if the point is in a string
     ((nth 3 pps)
      (maxima-complete-filename))
     ;; Otherwise, complete the symbol
     (t
      (maxima-complete-symbol)))))


(defun acm-backend-maxima-candidates (keyword)
  (nconc
   (mapcar (lambda (candidate)
             (list :key          candidate
                   :icon         "unknown"
                   :label        candidate
                   :displayLabel candidate
                   :annotation   "maxima"
                   :backend      "maxima"))
           (ignore-errors
             (maxima-get-completions
              keyword
              maxima-auxiliary-inferior-process)))
   ;; if point is in a string, complete library
   (when (nth 3 (parse-partial-sexp (maxima-form-beginning-position)
                                    (point)))
     (mapcar (lambda (candidate)
               (list :key          candidate
                     :icon         "package"
                     :label        candidate
                     :displayLabel candidate
                     :annotation   "maxima"
                     :backend      "maxima"))
               (maxima-get-libraries keyword t)))))


(defun acm-backend-maxima-candidate-doc (candidate)
  (let ((keyword (plist-get candidate :key)))
    (mapconcat (lambda (element) (format "%s" element))
               (maxima-document-get keyword maxima-auxiliary-inferior-process)
               "\n")))

(defun acm-update-candidates-append-maxima-results (fn)
  (if (or (eq major-mode 'maxima-mode)
          (eq major-mode 'maxima-inferior-mode))
      (let* ((keyword (acm-get-input-prefix))
             (maxima-res (acm-backend-maxima-candidates keyword)))
        (append maxima-res (funcall fn)))
    (funcall fn)))
(advice-add 'acm-update-candidates
            :around #'acm-update-candidates-append-maxima-results)

(add-hook 'maxima-mode-hook          #'lsp-bridge-mode)
(add-hook 'maxima-inferior-mode-hook #'lsp-bridge-mode)

;;; ob-maxima and maxima

(setf (cdr (assoc "maxima" org-babel-tangle-lang-exts #'string=)) "mac")

(setq org-babel-default-header-args:maxima ())

(defun org-babel-execute:maxima (body params)
  "Execute a block of Maxima code with Babel.
`BODY' is the contents of the block, as a string.
`PARAMS' is a property list containing the parameters of the block. "
  (let ((result-params (split-string (or (cdr (assq :results params)) ""))))
    (maxima-send-block-wait body)
    (maxima-last-output-noprompt maxima-inferior-process)))

(provide 'init-maxima)

;;; init-maxima.el ends here
