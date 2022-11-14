;;; init-elpa.el --- Config Elpa Source -*- lexical-binding: t -*-
;;; Commentary:

;; For speed up the connection in China, use USTC source.
;; And using use-package to config.

;;; Code:

(require 'package)
(package-initialize)

;; Set source: [refer: https://mirrors.ustc.edu.cn/help/elpa.html]
;; [2022-10-21] changed to emacs-china
;; https://elpamirror.emacs-china.org
(setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
                         ("melpa" . "http://1.15.88.122/melpa/")
                         ("nongnu" . "http://1.15.88.122/nongnu/")
                         ("org" . "http://1.15.88.122/org/")))

;; Update local cache
(package-refresh-contents)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; exec
(use-package exec-path-from-shell
             :if *is-macos*
             :config
             (exec-path-from-shell-initialize))
 
(provide 'init-elpa)
;;; init-elpa.el ends here
