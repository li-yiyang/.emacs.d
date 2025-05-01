;;; init-dired.el --- Setup for dired -*- lexical-binding: t -*-

;; dirvish --- replace dired
;; see https://github.com/alexluigit/dirvish
;;
;; requirement
;; + brew install coreutils fd poppler ffmpegthumbnailer mediainfo imagemagick

(require 'dirvish)
(require 'dirvish-fd)
(require 'dirvish-ls)
(require 'dirvish-quick-access)
(require 'dirvish-extras)
(require 'dirvish-yank)
(require 'dirvish-history)

(dirvish-override-dired-mode)

(defgroup ryo.dired ()
  "Ryo's configures for dired. "
  :prefix "ryo.dired:")

;; Extensions matching (see image-file.el)

(defcustom ryo.dired:video-file-name-extensions
  (purecopy '("mp4" "wav" "avi" "mov"))
  "A list of video-file filename extensions.
Filenames having one of these extensions are considered video files,
in addition to those matching `ryo.dired:video-file-name-regexps'. "
  :type  '(repeat string)
  :group 'ryo.dired)

(defcustom ryo.dired:video-file-name-regexps
  ()
  "A list of video-file filename extensions regexp.
Filenames having one of these extensions are considered video files,
in addition to those matching `ryo.dired:video-file-name-regexps'. "
  :type  '(repeat regexp)
  :group 'ryo.dired)

(defun ryo.dired:get-filename-image-p ()
  "Test if current dired file is a image file by filename. "
  (string-match-p (image-file-name-regexp)
                  (file-name-extension (dired-get-filename nil t) t)))

(defun ryo.dired:video-file-name-regexp ()
  "Retur a regular expression matching video filenames. "
  (let ((exts-regexp
         (and ryo.dired:video-file-name-extensions
              (concat "\\."
                      (regexp-opt
                       (append (mapcar #'upcase ryo.dired:video-file-name-extensions)
                               ryo.dired:video-file-name-extensions)
                       t)
                      "\\'"))))
    (mapconcat #'identity
               (delq nil (nconc (list exts-regexp)
                                (ensure-list ryo.dired:video-file-name-regexps)))
               "\\|")))

(defun ryo.dired:get-filename-video-p ()
  "Test if current dired file is a video file by filename. "
  (string-match-p (ryo.dired:video-file-name-regexp)
                  (file-name-extension (dired-get-filename nil t) t)))

;; Function Implementation
;;; Compress Toolbox

(defun ryo.dired:gzip-compress-dir ()
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (dir  (file-name-nondirectory file))
         (gzip (file-name-with-extension file "tar.gz"))
         (cmd  (concat "cd " dir " && tar czf " gzip " *")))
    (message "Run with: %s" cmd)
    (shell-command cmd)
    (revert-buffer-quick)))

(defun ryo.dired:tar-compress-dir ()
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (dir  (file-name-nondirectory file))
         (tar  (file-name-with-extension file "tar"))
         (cmd  (concat "cd " dir " && tar cf " tar " *")))
    (message "Run with: %s" cmd)
    (shell-command cmd)
    (revert-buffer-quick)))

(defun ryo.dired:zip-compress-dir ()
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (dir  (file-name-nondirectory file))
         (gzip (file-name-with-extension file "zip"))
         (cmd  (concat "cd " dir " && zip -R " file " *")))
    (message "Run with: %s" cmd)
    (shell-command cmd)
    (revert-buffer-quick)))

;;; ImageMagick Toolbox

(defun ryo.dired:imagemagick-convert-images (format)
  "Use `magick' command to convert between image format.

    (dolist (img images) magick <img> <img>.<output>)
"
  (interactive (list (completing-read "To: " image-file-name-extensions
                                      nil t nil)))
  (with-current-buffer (get-buffer-create "*IMAGEMAGICK*")
    (erase-buffer)
    (dolist (file (dired-get-marked-files nil))
      (when (string-match-p (image-file-name-regexp) file)
        (let* ((output (file-name-with-extension file format))
               (cmd    (concat "magick \"" (file-truename file) "\""
                               "\"" (file-truename output) "\"")))
          (message cmd)
          (shell-command cmd (current-buffer)))))))

(cl-defun ryo.dired:imagemagick-montage-images
    (files output &key (background "#FFFFFF") (geometry "+0+0"))
  "Use ImageMagick to merge images:

    magick montage -background <background> -geometry <geometry> <files> <output>"
  (with-current-buffer (get-buffer-create "*IMAGEMAGICK*")
    (erase-buffer)
    (let ((cmd (concat
                 "magick montage"
                 (when background
                   (format " -background '%s'" background))
                 (format " -geometry %s" geometry)
                 (string-join (mapcar (lambda (file)
                                        (format " \"%s\"" file))
                                      files)
                              " ")
                 (format " \"%s\"" output))))
      (message cmd)
      (shell-command cmd (current-buffer)))))

(cl-defun ryo.dired:imagemagick-image-size (image &optional (fmt "%wx%h"))
  "Use ImageMagick to get `image' size by `fmt'. "
  (with-current-buffer (get-buffer-create "*IMAGEMAGICK*")
    (erase-buffer)
    (let ((cmd (concat "magick identify -format \"" fmt "\" \""
                       (expand-file-name image) "\"")))
      (shell-command cmd (current-buffer))
      (buffer-string))))

(defun ryo.dired:imagemagick-resize-images (size images)
  "Use ImageMagick to resize images to size:

    magick <image> -resize <size> <image>

Parameters:
+ `size': magick resize param
+ `image': file name or (from to)"
  (with-current-buffer (get-buffer-create "*IMAGEMAGICK*")
    (erase-buffer)
    (dolist (image images)
      (let* ((from (if (listp image) (first  image) image))
             (to   (if (listp image) (second image) image))
             (cmd  (concat
                    "magick "
                    (format "\"%s\" " from)
                    (format "-resize %s " size)
                    (format "\"%s\""  to))))
        (message cmd)
        (shell-command cmd (current-buffer))))))

(defun ryo.dired:imagemagick-append-images (output geometry-x geometry-y)
  "For dired usage. "
  (interactive (list (let ((dir (dired-current-directory)))
                       (read-file-name "Merge To: "
                                       dir
                                       (expand-file-name "output.png" dir)
                                       nil))
                     (read-number "Geometry X: " 0)
                     (read-number "Geometry Y: " 0)))
  (ryo.dired:imagemagick-montage-images
   (dired-get-marked-files nil)
   (expand-file-name output (dired-current-directory))
   :geometry (format "+%d+%d" geometry-x geometry-y)))

(defcustom ryo.dired:imagemagick-ordered-dither-thresholds
  '("threshold" "1x1" "checks" "2x1" "o2x2" "2x2" "o3x3" "3x3" "o4x4" "4x4"
    "o8x8" "8x8" "h4x4a" "4x1" "h6x6a" "6x1" "h8x8a" "8x1" "h4x4o" "h6x6o"
    "h8x8o" "h16x16o" "c5x5b" "c5x5" "c5x5w" "c6x6b" "c6x6" "c6x6w" "c7x7b"
    "c7x7w" "c7x7")
  "All the ordered dither thresholds methods. "
  :group 'ryo.dired)

(defun ryo.dired:imagemagick-ordered-dither-images (ordered-dither)
  "ImageMagick ordered dither images:

    magick <from>.<ext> -ordered-dither <ordered-dither> <from>.dither.png
"
  (interactive (list (completing-read
                      "Ordered Dither Thresholds: "
                      ryo.dired:imagemagick-ordered-dither-thresholds
                      nil t "o")))
  (let ((files (dired-get-marked-files nil)))
    (with-current-buffer (get-buffer-create "*IMAGEMAGICK*")
      (erase-buffer)
      (dolist (file files)
        (let ((cmd (concat
                    "magick \"" file "\""
                    " -ordered-dither " ordered-dither
                    " \"" (file-name-with-extension file ".dither.png") "\"")))
          (message cmd)
          (shell-command cmd (current-buffer)))))))

(defun ryo.dired:imagemagick-ordered-dither-images-inplace (threshold)
  "ImageMagick ordered dither images:
magick -ordered-dither <threshold> <dirvish--marked-files> <same-as-input>"
  (interactive "sThreshold Method: \n")
  (cl-flet ((escape (filename)
              (concat "\"" (eshell-escape-arg filename)"\"")))
    (let ((cmds (mapcar (lambda (file)
                          (concat "magick "
                                  (escape file)
                                  "  -ordered-dither "
                                  threshold
                                  " "
                                  (escape file)))
                        (cl-remove-if-not
                         (lambda (file) (string-match-p (image-file-name-regexp)
                                                        (file-name-extension file)))
                         (dired-get-marked-files nil)))))
      (mapcar (lambda (cmd)
                (message cmd)
                (shell-command cmd))
              cmds)
      (revert-buffer-quick))))

(defun ryo.dired:open-externally ()
  (interactive)
  (when (eq system-type 'darwin)
    (shell-command (concat "open " (dired-get-filename nil t)))))

(cl-defun ryo:ffmpeg-video-vf (in to &key (vf nil vf-p) (message? t))
  "FFMPEG generation.

    ffmpeg -i <input> -o <input>.<to-type>

Arguments:
+ `in' should be input file pathname
+ `vf' should be video effect
+ `to' should be output file pathname
+ `message?' if non-nil, output `cmd' to *Message*
"
  (with-current-buffer (get-buffer-create "*FFMPEG*")
    (erase-buffer)
    (let ((cmd (concat "ffmpeg -y "
                       " -i \"" (file-truename in) "\" "
                       (when (and vf-p vf) (concat " -vf \"" vf "\" "))
                       " \"" (file-truename to) "\" ")))
      (when message? (message cmd))
      (shell-command cmd (current-buffer)))))

(defun ryo.dired:ffmpeg-convert-video (to-type vf)
  "ffmpeg -i <input> -o <input>.<to-type>"
  (interactive
   (list (completing-read "To: " ryo.dired:video-file-name-extensions
                          nil t nil)
         nil))
  (mapcar (lambda (file)
            (ryo:ffmpeg-video-vf file (file-name-with-extension file to-type)
                                 :vf vf))
          (dired-get-marked-files nil)))

;; Toolbox Menu

(transient-define-prefix ryo.dired:compress-toolbox-menu ()
  "Popup a Compress Toolbox menu. "
  [:description
   (lambda () (dirvish--format-menu-heading
               (concat "Compress Dir: " (dired-current-directory nil))
               (dirvish--marked-files-as-info-string)))
   ("g" "Compress with GZIP" ryo.dired:gzip-compress-dir)
   ("t" "Compress with TAR"  ryo.dired:tar-compress-dir)
   ("z" "Compress with ZIP"  ryo.dired:zip-compress-dir)])


(transient-define-prefix ryo.dired:imagemagick-ordered-dither-menu ()
  "Popup a ImageMagick Toolbox for Ordered Dither menu. "
  [:description
   (lambda () (dirvish--format-menu-heading
               "ImageMagick Ordered Dither"
               (dirvish--marked-files-as-info-string)))
   ("N" "Threshold by..."
    ryo.dired:imagemagick-ordered-dither-images)
   ("n" "Threshold 1x1 (non-dither)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-ordered-dither-images "1x1")))
   ("c" "Checkerboard 2x1 (dither)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-ordered-dither-images "2x1")))
   ("2" "Ordered 2x2 (dispersed)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-ordered-dither-images "o2x2")))
   ("3" "Ordered 3x3 (dispersed)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-ordered-dither-images "o3x3")))
   ("4" "Ordered 4x4 (dispersed)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-ordered-dither-images "o4x4")))
   ("8" "Ordered 8x8 (dispersed)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-ordered-dither-images "o8x8")))
   ])

(transient-define-prefix ryo.dired:imagemagick-toolbox-menu ()
  "Popup a ImageMagick Toolbox menu. "
  [:description
   (lambda () (dirvish--format-menu-heading
               "ImageMagick"
               (dirvish--marked-files-as-info-string)))
   ("a" "append selected images"   ryo.dired:imagemagick-append-images)
   ("D" "dither selected image(s)" ryo.dired:imagemagick-ordered-dither-menu)
   ("r" "resize selected image(s)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-resize-images
       (completing-read "Size: "
                        '("400x400" "400x400\\!" "400x400\\>" "400x400\\<"
                          "50%" "1024@")
                        nil nil)
       (dired-get-marked-files))))
   ("h" "resize selected image(s) to fit reference image height"
    (lambda () (interactive)
      (ryo.dired:imagemagick-resize-images
       (ryo.dired:imagemagick-image-size
        (read-file-name "Image height to fit: "
                        (dired-current-directory)
                        (dired-get-filename))
         "x%h")
        (dired-get-marked-files))))
   ("w" "resize selected image(s) to fit reference image width"
    (lambda () (interactive)
      (ryo.dired:imagemagick-resize-images
       (ryo.dired:imagemagick-image-size
        (read-file-name "Image width to fit: "
                        (dired-current-directory)
                        (dired-get-filename))
         "%wx")
        (dired-get-marked-files))))
   ("d" "dither quick (o4x4)"
    (lambda () (interactive)
      (ryo.dired:imagemagick-ordered-dither-images "o4x4")))
   ])

(transient-define-prefix ryo.dired:ffmpeg-toolbox-menu ()
  "Popup a FFmpeg Toolbox menu. "
  [:description
   (lambda () (dirvish--format-menu-heading
               "FFmpeg"
               (dirvish--marked-files-as-info-string)))
   ("c" "convert video quick"      ryo.dired:ffmpeg-convert-video)
   ;; ("C" "concat  video"            ryo.dired:ffmpeg-concat-videos)
   ("g" "convert video to GIF (fps=6,width=800)"
    (lambda () (interactive)
      (ryo.dired:ffmpeg-convert-video "gif" "fps=6,scale=800:-1:flags=lanczos")))
   ])

(transient-define-prefix ryo.dired:dirvish-file-toolbox-menu ()
  "Popup a Toolbox menu. "
  [:description
   (lambda () (dirvish--format-menu-heading
               "Apply on File"
               (dirvish--marked-files-as-info-string)))
   ("F" "Dirvish File Info"   dirvish-file-info-menu)
   ("o" "Open externally"     ryo.dired:open-externally)
   ("c" "Compress Toolbox"    ryo.dired:compress-toolbox-menu
    :if (lambda () (not (dired-nondirectory-p (dired-get-filename nil t)))))
   ("i" "ImageMagick Toolbox" ryo.dired:imagemagick-toolbox-menu
    :if ryo.dired:get-filename-image-p)
   ("f" "FFmpeg Toolbox"      ryo.dired:ffmpeg-toolbox-menu
    :if ryo.dired:get-filename-video-p)
   ])

;;;;

(cl-defmacro extcase (file-name &body cases)
  "Case by `file-name' extension.
Return `t' if matches, otherwise `nil'.

Syntax:
 * `file-name': expr for file-name
 * `cases': case should be like (ext-type . progn)
   + `ext-type' can be
     + string for single ext pattern
     + list of string for multiple ext pattern
     + `:otherwise' (should be the last case)
       for otherwise
"
  (let ((ext-name (gensym "EXT-NAME")))
    `(let ((,ext-name (file-name-extension ,file-name)))
       (cond
        ,@(cl-loop for (ext-type* . progn) in cases
                   if (listp ext-type*)
                   collect `((or ,@(mapcar (lambda (ext-type)
                                             `(string= ,ext-name ,ext-type))
                                           ext-type*))
                             ,@progn
                             t)
                   else if (stringp ext-type*)
                   collect `((string= ,ext-name ,ext-type*) ,@progn)
                   else if (eq ext-type* :otherwise)
                   collect `(t ,@progn nil)
                   while (not (eq ext-type* :otherwise)))))))

;; tar xzf tar.gz

(defun ryo.dired:dired-uncompress-file ()
  "In Dired, uncompress this file (if it is a tar file) right here. "
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (when (extcase file-name
                   (("gz" "tar")
                    (message (concat "Untar " file-name " ..."))
                    (shell-command (concat "tar xzf " file-name)))
                   ("zip"
                    (message (concat "Unzip " file-name " ..."))
                    (shell-command (concat "unzip " file-name)))
                   (:otherwise
                    (message "Not a known compressed file")))
      (message (concat "Finish uncompress " file-name))
      (revert-buffer-quick))))

(setq delete-by-moving-to-trash t)
(setq dirvish-use-mode-line nil)

;; store dirvish cache to privates

(setq dirvish-cache-dir
      (expand-file-name "privates/dirvish/" user-emacs-directory))

;; Following Config is copied from dirvish sample config
;; https://github.com/alexluigit/dirvish/blob/119f9f59a618bb7b476c93e9ab1d7542c5c1df41/docs/CUSTOMIZING.org#sample-config

;; dirvish attribution

(setq dirvish-attributes '(collapse subtree-state vc-state git-msg))

(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")

;; use dirvish-fd to find file

(bind-key (kbd "C-c f") #'dirvish-fd)

(define-key dirvish-mode-map (kbd "a")   #'dirvish-quick-access)
(define-key dirvish-mode-map (kbd "y")   #'dirvish-yank-menu)
(define-key dirvish-mode-map (kbd "N")   #'dirvish-fd)
(define-key dirvish-mode-map (kbd "s")   #'dirvish-quicksort)
(define-key dirvish-mode-map (kbd "v")   #'dirvish-vc-menu)
(define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
(define-key dirvish-mode-map (kbd "M-t") #'dirvish-layout-toggle)
(define-key dirvish-mode-map (kbd "M-m") #'dirvish-mark-menu)
(define-key dirvish-mode-map (kbd "b")   #'dirvish-history-go-backward)
(define-key dirvish-mode-map (kbd "^")   #'dired-up-directory)
(define-key dirvish-mode-map (kbd "RET") #'dired-find-file-other-window)
(define-key dirvish-mode-map (kbd "X")   #'ryo.dired:dired-uncompress-file)
(define-key dirvish-mode-map (kbd "f")   #'ryo.dired:dirvish-file-toolbox-menu)

;; mouse support

(setq dired-mouse-drag-files t)
(setq mouse-drag-and-drop-region-cross-program t)

(setq mouse-1-click-follows-link nil)
(define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
(define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)

;; macos ls fix
;; see https://github.com/alexluigit/dirvish/blob/119f9f59a618bb7b476c93e9ab1d7542c5c1df41/docs/CUSTOMIZING.org#listing-directory-failed-but-access-file-worked-error-on-macos

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

;; W opens file

(require 'image-mode)
(when (eq system-type 'darwin)
  (define-key image-mode-map (kbd "W")
              (lambda () (interactive)
                (let ((file (buffer-file-name)))
                  (when file
                    (shell-command (format "open \"%s\"" file)))))))

(provide 'init-dired)

;;; init-dired.el ends here
