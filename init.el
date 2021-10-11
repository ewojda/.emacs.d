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

;;-----Languages-----

(use-package lsp-mode)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'bottom))

;;Go

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))

;;Web

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;;-----End Languages-----

;;Git

(use-package magit)

;;Tab width

(setq default-tab-width 4)
(setq c-basic-offset 4)

;;Tabs

(tab-bar-mode 1)

;;Theme

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;;Scrolling
(setq scroll-preserve-screen-position t)
(scroll-bar-mode 1)
(setq ew-scroll-lines-amount 5)

(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)
(global-set-key [next] (lambda () (interactive) (scroll-up ew-scroll-lines-amount)))
(global-set-key [prior] (lambda () (interactive) (scroll-down ew-scroll-lines-amount)))

;;Other
;;  Disable line wrapping
(set-default 'truncate-lines t)
;;  Enable functions disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;  Disable electric-indent-mode
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
