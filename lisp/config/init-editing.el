;;; init-editing.el --- Init some basic editing perferance.

;; Basic editing

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(defgroup ryo.edit ()
  "Some basic editting configures. "
  :prefix "ryo.edit:")

(defun ryo.edit:gbk-to-utf-8 ()
  "Change buffer encoding from GBK to UTF-8. "
  (interactive)
  (revert-buffer-with-coding-system 'gbk)
  (set-buffer-file-coding-system    'utf-8))

(defun ryo.edit:tab-width-2 ()
  "Set Tab width 2 for those obstinate mode. "
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2))

;; Separedit
;; see https://github.com/twlz0ne/separedit.el

(require 'separedit)

(define-key prog-mode-map (kbd "C-c '") #'separedit)
(setq separedit-default-mode                'text-mode)
(setq separedit-code-block-default-mode     'text-mode)
(setq separedit-preserve-string-indentation nil)
(setq separedit-continue-fill-column        nil)

;; Manual save

(defvar ryo.edit:before-manual-save-hook nil
  "Hook called before function `ryo.edit:manual-save'. ")

(defun ryo.edit:manual-save ()
  "Manual Save.

This shall be bind with `C-x C-s' to differ with
auto save (see `init-autosave.lisp' for config)"
  (interactive)
  (run-hooks 'ryo.edit:before-manual-save-hook)
  (save-buffer))

(global-set-key (kbd "C-x C-s") #'ryo.edit:manual-save)

;; Project structures

(defun ryo.edit:update-last-upated ()
  "Update Last-Updated infomation. "
  (when (buffer-file-name)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Last-Updated: [[:digit:]]\\{4\\}-[[:digit:]]\\{1,2\\}-[[:digit:]]\\{1,2\\} [[:digit:]]\\{1,2\\}:[[:digit:]]\\{1,2\\}" nil t)
        (replace-match
         (concat "Last-Updated: " (format-time-string "%Y-%m-%d %H:%M")))))))

(add-hook 'ryo.edit:before-manual-save-hook #'ryo.edit:update-last-upated)

;; Large file editing support:
;; from https://emacs-china.org/t/topic/25811/9

(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; GhostScripts Support
;; atomic-chrome
;; see: https://github.com/alpha22jp/atomic-chrome

(require 'atomic-chrome)
(atomic-chrome-start-server)


(provide 'init-editing)

;;; init-editing.el ends here
