;;; init-ruby.el --- Ruby Config -*- lexical-binding: t -*-
;;; Commentary:

;; referring: https://ithelp.ithome.com.tw/articles/10204657

;;; Code:

;;; Ruby Part
(use-package ruby-mode
  :mode ("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'"
	 "\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'")
  :config
  (use-package rvm
    :ensure t
    :config
    (rvm-use-default))
  (add-hook 'ruby-mode-hook (lambda ()
			      (add-to-list (make-local-variable 'company-backends)
					   '(company-robe)))))

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  )

(use-package seeing-is-believing
  :config
  (add-hook 'ruby-mode-hook 'seeing-is-believing)
  (setq seeing-is-believing-prefix "C-."
	seeing-is-believing-max-length 150
	seeing-is-believing-max-results 10)
  ())

(use-package ruby-electric
  :ensure t
  :hook (ruby-mode . ruby-electric-mode)
  )

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode)
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
  )

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode)
  )
  
(use-package rufo
  :ensure t
  :hook (ruby-mode . rufo-minor-mode)
  )

;;; Rails Part
(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  (setq projectile-rails-expand-snippet nil)
  )

(provide 'init-ruby)
;;; init-ruby.el ends here
