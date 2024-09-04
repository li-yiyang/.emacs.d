;;; init-json.el --- JSON for emacs

;; JSON mode
;; see https://github.com/json-emacs/json-mode
;; depends on
;; https://github.com/Sterlingg/json-snatcher

(require 'json-mode)

;; by default use 2 space for indent

(setq js-indent-level 2)

;; enable electric pair mode in JSON mode
(add-hook 'json-mode-hook #'electric-pair-mode)

(provide 'init-json)

;;; init-json.el ends here
