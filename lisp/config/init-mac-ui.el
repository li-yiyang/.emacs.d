;;; init-mac-ui.el --- Init for mac UI (emacs-plus)

;; This should be required in early loading.
;;
;; The pixelwise, undecrated window and the auto theme switch is provided
;; by emacs-plus.
;;
;; see https://github.com/d12frosted/homebrew-emacs-plus for details
;;
;; The dark and light theme is provided by tao-theme
;;
;; see https://github.com/11111000000/tao-theme-emacs for details
;;

(require 'tao-theme)
(require 'tao-yang-theme)
(require 'tao-yin-theme)

;; pixelwise load
(setq frame-resize-pixelwise t)

;; undecrated window
(add-to-list 'default-frame-alist '(undecorated-round . t)) ; rounded corners

;; or
;; (add-to-list 'default-frame-alist '(undecorated . t)) ; squared corners

;; dark and light theme switch

(defcustom ryo.ui:mac-dark-theme 'tao-yin
  "The dark theme. "
  :group 'ryo.ui)

(defcustom ryo.ui:mac-light-theme 'tao-yang
  "The light theme. "
  :group 'ryo.ui)

(custom-set-faces '(highlight ((t :inverse-video t))))

(defun ryo.ui:mac-apply-theme (appearance)
  "Switch emacs theme according to `appearance'. "
  (mapc #'disable-theme custom-enabled-themes) ; unload all
  (pcase appearance
    ('light (load-theme ryo.ui:mac-light-theme t))
    ('dark  (load-theme ryo.ui:mac-dark-theme  t))))

(add-hook 'ns-system-appearance-change-functions #'ryo.ui:mac-apply-theme)

(provide 'init-mac-ui)

;;; init-mac-ui.el ends here
