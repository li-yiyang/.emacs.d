;;; init-eshell.el --- Set up eshell -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'eshell)
(require 'emojishell)

;; by default using ssh

(setq tramp-default-method "ssh")

;; kill visual buffers

(setq eshell-destroy-buffer-when-process-dies t)

;; history files saved to privates

(setq eshell-history-file-name
      (expand-file-name "privates/eshell/history" user-emacs-directory))
(setq eshell-last-dir-ring-file-name
      (expand-file-name "privates/eshell/lastdir" user-emacs-directory))

;; use emojishell

(setq eshell-prompt-function #'emojishell-emoji-prompt)

;; default clear command clears the buffer
;; using `eshell/clear-scrollback', aka `(eshell/clear t)' by default

(defun ryo.eshell:clear-advice (args)
  (if (null args) '(t) args))
(advice-add 'eshell/clear :filter-args #'ryo.eshell:clear-advice)

;; get and list eshell buffer
;; inspired from https://emacs-china.org/t/eshell-util/24432/3

(defun ryo.eshell:eshell-buffers ()
  "Return a list of eshell buffers"
  (let ((ebuff '()))
    (cl-dolist (buff (buffer-list))
      (with-current-buffer buff
  (when (derived-mode-p 'eshell-mode)
    (push buff ebuff))))
    ebuff))

;; default open new eshell buffer when calling `eshell'
;; see https://www.emacswiki.org/emacs/EshellMultipleEshellBuffers
;; advice function wrap around for interactive call

(defun ryo.eshell:eshell-advice (fn &rest args)
  (if args (funcall fn args)
    ;; by default open new eshell on *different* dir
    (let* ((cwd default-directory)
           (buff (cl-find-if
                  #'(lambda (buff)
                      (string= cwd (buffer-local-value 'default-directory buff)))
                  (ryo.eshell:eshell-buffers))))
      (if buff
          (switch-to-buffer buff)
        (funcall fn '(N))))))
(advice-add 'eshell :around #'ryo.eshell:eshell-advice)

;; eshls: list Eshell buffer
(defun eshell/eshls (&rest args)
  "List all eshell buffer names.
Click to switch to eshell buffer. "
  (cl-loop for buffer in (buffer-list)
           if (eq (buffer-local-value 'major-mode buffer) 'eshell-mode)
           do (eshell-printn
               (propertize
                (buffer-name buffer)
                'category 'default-button
                'button   (buffer-name buffer)
                'action   #'(lambda (&rest args)
                              (interactive)
                              (let ((buffer (get-text-property (point) 'button)))
                                (message buffer)
                                (switch-to-buffer buffer)))
                'help-echo   (concat "Switch to eshell buffer: `"
                                     (buffer-name buffer) "'. ")
                'follow-link t))))

;; imgcat: show image in eshell
;; see https://emacs-china.org/t/imgcat-eshell/3439
(defun eshell/imgcat (&rest args)
  "Display `images' in eshell. "
  (if eshell-in-pipeline-p
      (error "Elisp function does not support piped input. ")
    (let (width height max-width max-height scale)
      (eshell-eval-using-options
       "imgcat" args
       '((nil "width"      t width      "width of image(s)")
         (nil "height"     t height     "height of image(s)")
         (nil "max-width"  t max-width  "max width of image(s) [default 400]")
         (nil "max-height" t max-height "max height of image(s)")
         (?h  "help"   nil nil "show this usage screen")
         :show-usage
         :usage "[OPTION] IMAGE...
Show IMAGE(s) file in eshell. ")
       ;; TODO: better arg parse
       (let ((property `(,@(when max-width
                             (list :max-width (string-to-number max-width)))
                         ,@(when max-height
                             (list :max-height (string-to-number max-height)))
                         ,@(when (and width
                                      (or (not max-width)
                                          (< (string-to-number width)
                                             (string-to-number max-width))))
                             (list :width (string-to-number width)))
                         ,@(when (and height (or (not max-height)
                                                 (< (string-to-number height)
                                                    (string-to-number max-height))))
                             (list :height (string-to-number height))))))
         (when (endp property)
           (setf property '(:max-width 400)))
         (if (endp args)
             (eshell-show-usage "image" nil)
           (dolist (img (eshell-flatten-list args))
             (eshell-printn
              (propertize
               img
               'display (apply #'create-image (expand-file-name img) nil nil property))))))))))

(provide 'init-eshell)

;;; init-eshell.el ends here
