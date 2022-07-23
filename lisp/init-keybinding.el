;;; init-keybinding --- Load the Key Binding -*- lexical-binding: t -*-
;;; Commentary:

;; I'm using macOS... (Not very easy to use.)
;; This config mainly refers https://book.emacs-china.org/#org8bf1657

;;; Code:

(when *is-macos*
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

(global-set-key (kbd "s-a") 'mark-whole-buffer) ;; C-a: Select All
(global-set-key (kbd "s-c") 'kill-ring-save) ;; C-c: Copy Select
(global-set-key (kbd "s-s") 'save-buffer) ;; C-s: Save File
(global-set-key (kbd "s-v") 'yank) ;; C-v: Paste
(global-set-key (kbd "s-z") 'undo) ;; C-z: Undo
(global-set-key (kbd "s-x") 'kill-region) ;; C-x: Cut Select

(provide 'init-keybinding)
;;; init-keybinding.el ends here
