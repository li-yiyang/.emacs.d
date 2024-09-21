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

(setq lsp-bridge-enable-hover-diagnostic t)

;; my own lsp server support and config
(setq lsp-bridge-user-langserver-dir
      (expand-file-name "lspserver" user-emacs-directory))

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
  (require 'acm-terminal))

(provide 'init-lsp)

;;; init-lsp.el ends here
