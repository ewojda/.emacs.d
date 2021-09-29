(load "~/.emacs.d/sanemacs.el" nil t)

;;Autocomplete
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package counsel)
(ivy-mode 1)

;;Languages
;;  LSP
(use-package lsp-mode)
(use-package lsp-ui)
;;  Go
(use-package go-mode)
(add-hook 'go-mode-hook 'lsp-deferred)

;;Git
(use-package magit)

;;Tab width
(setq default-tab-width 4)
(setq c-basic-offset 4)

;;Tabs
(tab-bar-mode 1)

;;Theme
(use-package monokai-theme)
(load-theme 'monokai t)

;;Scrolling
(setq scroll-preserve-screen-position t)
(scroll-bar-mode 1)
(setq ew-scroll-lines-amount 5)

(global-set-key (kbd "C-v") (lambda () (interactive) (scroll-up ew-scroll-lines-amount)))
(global-set-key (kbd "M-v") (lambda () (interactive) (scroll-down ew-scroll-lines-amount)))
(global-set-key [next] (lambda () (interactive) (scroll-up ew-scroll-lines-amount)))
(global-set-key [prior] (lambda () (interactive) (scroll-down ew-scroll-lines-amount)))

;;Other
(electric-pair-mode 1)
