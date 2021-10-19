(load "~/.emacs.d/sanemacs.el" nil t)

;;Autocomplete
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package counsel
  :config
  (ivy-mode 1))

;;Parens
(use-package smartparens
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config))

;;-----Code-----
;;  LSP
(use-package lsp-mode)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'bottom))
;;  Go
(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))
;;  Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
;;  Git
(use-package magit)

;;Theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;;Scrolling
(setq scroll-preserve-screen-position t)
(scroll-bar-mode 1)

(setq ew-scroll-lines-vertical-amount 5)
(setq ew-scroll-lines-horizontal-amount 20)

(global-set-key (kbd "<next>") (lambda () (interactive) (scroll-up ew-scroll-lines-vertical-amount)))
(global-set-key (kbd "<prior>") (lambda () (interactive) (scroll-down ew-scroll-lines-vertical-amount)))
(global-set-key (kbd "C-<next>") (lambda () (interactive) (scroll-left ew-scroll-lines-horizontal-amount)))
(global-set-key (kbd "C-<prior>") (lambda () (interactive) (scroll-right ew-scroll-lines-horizontal-amount)))

;;Other
;;  Set tab width
(setq default-tab-width 4)
(setq c-basic-offset 4)
;;  Enable tab bar
(tab-bar-mode 1)
;;  Paste with C-v
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)
;;  Disable line wrapping
(set-default 'truncate-lines t)
;;  Enable functions disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;  Disable electric-indent-mode
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
;;  Make tab insert literal tab
(global-set-key [tab] 'tab-to-tab-stop)
;;  Autoindent with S-tab
(global-set-key [backtab] 'indent-for-tab-command)
