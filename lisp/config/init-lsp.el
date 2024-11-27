;;; init-lsp.el --- Set up Language Server Protocol.

;; This file setup the LSP.

;; LSP-bridge Installation
;; see https://github.com/manateelazycat/lsp-bridge/ for details
;;
;; + Python requirements
;;   pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz
;; + Emacs requirements
;;   + markdown-mode
;;     https://github.com/jrblevin/markdown-mode

(require 'markdown-mode)

;;   + yasnippet
;;     https://github.com/joaotavora/yasnippet
;;     see init-yas.el for details

(require 'init-yas)

;; LSP-bridge Configuration

(require 'lsp-bridge)

;; hover and popout diagnostic

(setq lsp-bridge-enable-hover-diagnostic t)

;; tramp support by default

(setq lsp-bridge-enable-with-tramp       t)

;; my own lsp server support and config

(setq lsp-bridge-user-langserver-dir
      (expand-file-name "lspserver/" user-emacs-directory))

;; no tab and no return to complete, use only C-m to complete

(define-key acm-mode-map [tab] nil)
(define-key acm-mode-map "\t"  nil)
(define-key acm-mode-map "\n"  nil)

;; enable lsp-bridge globally

(global-lsp-bridge-mode)

;; acm-terminal
;; lsp-bridge use acm for completion, this is for TUI usage
;; see https://github.com/twlz0ne/acm-terminal
;;
;; Emacs requirements:
;; + popon
;;   https://github.com/twlz0ne/acm-terminal.git

(unless (display-graphic-p)
  (require 'popon)
  (require 'acm-terminal)

  ;; make it able to switch faces
  (defun ryo.ui:acm-terminal-patches ()
    "acm-terminal theme patches under TUI"
    (set-face-background 'acm-terminal-default-face
                         (face-attribute 'default :background))
    (set-face-background 'acm-terminal-select-face
                         (face-attribute 'highlight :background))
    (set-face-foreground 'acm-terminal-select-face
                         (face-attribute 'highlight :foreground)))
  (add-hook 'ryo.ui:after-tui-theme-loaded-hook
            #'ryo.ui:acm-terminal-patches))

(provide 'init-lsp)

;;; init-lsp.el ends here
