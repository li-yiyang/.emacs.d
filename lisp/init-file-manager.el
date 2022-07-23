;;; init-file-manager.el --- File Manager -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://zhuanlan.zhihu.com/p/156907392

;;; Code:
;; C-x C-j use command: dired-jump, open the dir of buffer now
(use-package all-the-icons)

(use-package neotree
  :init (setq neo-window-fixed-size nil
	      neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind (("<f8>" . neotree-toggle)))

(provide 'init-file-manager)
;;; init-file-manager.el ends here
