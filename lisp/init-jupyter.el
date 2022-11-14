;;; init-jupyter.el --- Jupyter Interface -* lexical-binding: t -*-
;;; Commentary:

;; referring: https://github.com/nnicandro/emacs-jupyter
;; referring: https://github.com/millejoh/emacs-ipython-notebook
;; referring: https://tam5917.hatenablog.com/entry/2021/03/28/204747

;;; Code:
(eval-when-compile
  (require 'ein)
  (require 'ein-notebook)
  (require 'ein-notebooklist)
  (require 'ein-markdown-mode)
  (require 'smartrep))

;; (add-hook 'ein:notebook-mode-hook 'electric-pair-mode) ;; お好みで
;; (add-hook 'ein:notebook-mode-hook 'undo-tree-mode) ;; お好みで

;; undoを有効化 (customizeから設定しておいたほうが良さげ)
(setq ein:worksheet-enable-undo t)

;; 画像をインライン表示 (customizeから設定しておいたほうが良さげ)
(setq ein:output-area-inlined-images t)
(setq ein:output-area-inlined-images t)
(setq ein:slice-image t)

;; markdownパーサー
;; M-x ein:markdown →HTMLに翻訳した結果を*markdown-output*バッファに出力
(require 'ein-markdown-mode)

;; pandocと markdownコマンドは入れておく
;; brew install pandoc
;; brew install markdown
(setq ein:markdown-command "pandoc --metadata pagetitle=\"markdown preview\" -f markdown -c ~/.pandoc/github-markdown.css -s --self-contained --mathjax=https://raw.githubusercontent.com/ustasb/dotfiles/b54b8f502eb94d6146c2a02bfc62ebda72b91035/pandoc/mathjax.js")

;; markdownをhtmlに出力してブラウザでプレビュー
(defun ein:markdown-preview ()
  (interactive)
  (ein:markdown-standalone)
  (browse-url-of-buffer ein:markdown-output-buffer-name))

;; smartrepを入れておく。
;; C-c C-n C-n C-n ... で下のセルに連続で移動、
;; その途中でC-p C-p C-pで上のセルに連続で移動など
;; セル間の移動がスムーズになってとても便利
(declare-function smartrep-define-key "smartrep")
(with-eval-after-load "ein-notebook"
  (smartrep-define-key ein:notebook-mode-map "C-c"
    '(("C-n" . 'ein:worksheet-goto-next-input-km)
      ("C-p" . 'ein:worksheet-goto-prev-input-km))))

; (use-package jupyter)
; (use-package ein
;   :config
;   (require 'ein-notebook)
;   (require 'ein-notebooklist)
;   (require 'ein-markdown-mode)
;   (require 'smartrep)
;   (setq ein:worksheet-enable-undo t)
;   (setq ein:output-area-inlined-images t)
;   (setq ein:markdown-command "pandoc --metadata pagetitle=\"Jupter NoteBook\" -f markdown -c ~/.pandoc/github-markdown.css -s --self-contained --mathjax=https://raw.githubusercontent.com/ustasb/dotfiles/b54b8f502eb94d6146c2a02bfc62ebda72b91035/pandoc/mathjax.js")
;   (defun ein:markdown-preview ()
;     (interactive)
;     (ein:markdown-standalone)
;     (browse-url-of-buffer ein:markdown-ouput-buffer-name))
;   (declare-function smartrep-define-key "smartrep"))
; 
; ;; note: github-markdown.css is from https://gist.github.com/dashed/6714393

(provide 'init-jupyter)
;;; init-jupyter.el ends here
