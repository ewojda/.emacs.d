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
;;  Lua
(use-package lua-mode
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (setq lua-indent-level 4)
  (add-hook 'lua-mode-hook #'lsp-deferred))

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

;;Indentation
(defun ew-newline-and-indent (&optional arg)
  "Insert a newline, then indent exactly as last line.
Adapted from `newline-and-indent'

With ARG, perform this action that many times."
  (interactive "*p")
  (unless arg
    (setq arg 1))
  (dotimes (_ arg)
	(let* (
		   (bol (line-beginning-position))
		   (eol (line-end-position))
		   (line (buffer-substring-no-properties bol eol)))
	  (string-match "\\(^[\t ]*\\)" line)
	  (insert "\n")
	  (insert (match-string 1 line)))))
	  
(defun ew-enable-custom-indentation () (interactive)
  ;;  Make tab insert literal tab
  (local-set-key [tab] 'tab-to-tab-stop)
  ;;  Autoindent with S-tab
  (local-set-key [backtab] 'indent-for-tab-command)
  ;;  Autoindent on newline
  (local-set-key (kbd "<return>") 'ew-newline-and-indent))
  
;;  Override tab and return bindings only in prog-mode
(add-hook 'prog-mode-hook 'ew-enable-custom-indentation)
;;  Disable electric-indent-mode
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
;;  Dont eat whitespace on save
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

;;Other
;;  Set tab width
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)
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
(put 'erase-buffer 'disabled nil)
;;  Drag text with alt-up/down
(use-package drag-stuff
  :config
  (global-set-key (kbd "M-<up>") (lambda () (interactive) (drag-stuff-up 1)))
  (global-set-key (kbd "M-<down>") (lambda () (interactive) (drag-stuff-down 1))))
;;  Set default mode to prog-mode
(setq-default major-mode 'prog-mode)
