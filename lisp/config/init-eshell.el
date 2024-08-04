;;; init-eshell.el --- Set up eshell

(require 'cl-lib)
(require 'eshell)
(require 'emojishell)

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

(provide 'init-eshell)

;;; init-eshell.el ends here
