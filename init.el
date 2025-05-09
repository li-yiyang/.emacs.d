;;; init.el --- Ryo's emacs configuration. -*- lexical-binding: t -*-

;;; This is main entrance of ryo's emacs configuration.

(setq debug-on-error t)

(require 'cl-lib)

;; Copied from lazy-emacs/site-start.el
;; see https://github.com/manateelazycat/lazycat-emacs/blob/master/site-start.el

(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

;; copied from https://www.reddit.com/r/emacs/comments/mw6idu/comment/gvhlnuw/

(cl-defmacro ryo:minibuffer-shut-up! (&body body)
  "Shut up minibuffer message output"
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

;; need to be loaded first otherwise will load original org and transient

(dolist (extension '("lisp/extensions/org-mode/lisp"
                     "lisp/extensions/transient/lisp"
                     ;; "lisp/extensions/maxima"
                     ))
  (add-to-list 'load-path (expand-file-name extension user-emacs-directory)))

;; ~/.emacs.d/lisp/... for the elisp files

(defun ryo:refresh-submodules ()
  "Refresh submodules. "
  (interactive)
  (add-subdirs-to-load-path (expand-file-name "lisp" user-emacs-directory))
  t)

(ryo:refresh-submodules)

(setq
 ;; 不要缩放frame.
 frame-inhibit-implied-resize t
 ;; 不要自动启用package
 package-enable-at-startup nil
 package--init-file-ensured t)

(require 'init-ui)
(require 'init-editing)
(require 'init-dired)
(require 'init-autosave)
(require 'init-recentf)

;; (require 'init-blink-search)
(require 'init-vertico)
(require 'init-lsp)

(when (file-exists-p (expand-file-name "lisp/privates/init-privates.el"
                                       user-emacs-directory))
  (require 'init-privates))

(require 'init-mail)
(require 'init-git)
(require 'init-lisp)
(require 'init-org)
(require 'init-latex)
(require 'init-eshell)
(require 'init-c-c++)
(require 'init-json)
(require 'init-python)
(require 'init-dot)
(require 'init-markdown)
(require 'init-web)
;; (require 'init-maxima)
(require 'init-artist)


;;; init.el ends here
