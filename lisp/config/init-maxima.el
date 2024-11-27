;;; init-maxima.el --- Maxima interface

(require 'maxima)

(add-to-list 'auto-mode-alist        '("\\.mac" . maxima-mode))
(add-to-list 'interpreter-mode-alist '("maxima" . maxima-mode))

(define-key maxima-mode-map (kbd "M-h")   nil)
(define-key maxima-mode-map (kbd "M-h f") #'maxima-help-at-point)
(define-key maxima-mode-map (kbd "M-h v") #'maxima-help-at-point)

;; Copied from init-lisp.el
(defun acm-backend-maxima-candidates (keyword)
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
             maxima-auxiliary-inferior-process))))

(defun acm-backend-maxima-candidate-doc (candidate)
  (let ((keyword (plist-get candidate :key)))
    (car (maxima-document-get
          keyword
          maxima-auxiliary-inferior-process))))

(defun acm-update-candidates-append-maxima-results (fn)
  (if (or (eq major-mode 'maxima-mode)
          (eq major-mode 'maxima-inferior-mode))
      (let* ((keyword (acm-get-input-prefix))
             (maxima-res (acm-backend-maxima-candidates keyword)))
        (append maxima-res (funcall fn)))
    (funcall fn)))
(advice-add 'acm-update-candidates
            :around #'acm-update-candidates-append-maxima-results)

(add-hook 'maxima-mode-hook #'lsp-bridge-mode)

(provide 'init-maxima)

;;; init-maxima.el ends here
