;;; init-elpa.el --- Config Elpa Source -*- lexical-binding: t -*-
;;; Commentary:

;; For speed up the connection in China, use USTC source.
;; And using use-package to config.

;;; Code:

(require 'package)
(package-initialize)

;; Set source: [refer: https://mirrors.ustc.edu.cn/help/elpa.html]
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

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
