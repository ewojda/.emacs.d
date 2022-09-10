;;* Sanemacs
(load "~/.emacs.d/sanemacs.el" nil t)
;;* Indentation
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

(defun ew-puttab () (interactive) (insert "\t"))

(defun ew-fix-indentation () (interactive)
	   ;;  Make tab insert literal tab
	   ;; (local-set-key [tab] 'tab-to-tab-stop)
	   (local-set-key [tab] 'ew-puttab)
	   ;;  Autoindent with S-tab
	   (local-set-key [backtab] 'indent-for-tab-command)
	   ;;  Autoindent on newline
	   (local-set-key (kbd "<return>") 'ew-newline-and-indent)
	   ;;  Disable electric-indent-mode
	   (when (fboundp 'electric-indent-mode) (electric-indent-mode -1)))

;;  Override tab and return bindings only in prog-mode
(add-hook 'prog-mode-hook 'ew-fix-indentation)
;;  Dont eat whitespace on save
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
;;* Completion
;;** Snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))
;;** Parens
(use-package smartparens
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config))
;;** Text completion
;;*** General
(use-package company
  :custom
  (company-minimum-prefix-length 0)
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
;;*** Programming
;;**** LSP
(use-package lsp-mode
  :mode "(\\.go\\')|(\\.lua\\')"
  :config
  (global-set-key (kbd "M-o") (lambda () (interactive) (lsp-hover))))
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-position 'bottom))

;;**** Go
(use-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))
;;**** Web
(use-package web-mode
  :config
  (web-mode-use-tabs)
  :custom
  (web-mode-enable-auto-indentation nil)
  (web-mode-code-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-markup-indent-offset 4)
  (web-mode-attr-value-indent-offset 4)
  (web-mode-attr-indent-offset 4)
  (web-mode-style-padding 4)
  (web-mode-script-padding 4)
  (web-mode-part-padding 4)
  (web-mode-block-padding 4)
  :mode "\\.html?\\'")
;;**** Git
(use-package magit
  :defer t)
;;**** Lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  ;;:custom
  ;;(lsp-clients-lua-language-server-command "lua-language-server")
  ;;(lsp-clients-lua-language-server-bin "/run/current-system/sw/bin/lua-language-server")
  ;;(lsp-clients-lua-language-server-install-dir "/run/current-system/sw/share/lua-language-server")
  ;;(lsp-clients-lua-language-server-main-location "/run/current-system/sw/share/lua-language-server/main.lua")
  :config
  ;;(add-hook 'lua-mode-hook #'lsp-deferred)
  (setq lua-indent-level 4))
;;**** Haxe
;;(use-package haxe-mode
;;  :mode "\\.hx\\'"
;;  :interpreter "haxe"
;;  :config
;;  (setq haxe-mode-map nil)
;;  (add-hook 'haxe-mode-hook 'ew-enable-custom-indentation)
;;  (add-hook 'haxe-mode-hook 'linum-mode))
(add-to-list 'load-path (expand-file-name "funda-haxe-mode" user-emacs-directory))
(use-package funda-haxe-mode
  :mode "\\.hx\\'"
  :interpreter "haxe"
  :ensure nil)
;;**** Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;;**** C#
(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  )
;;**** Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lua . t)
   (gnuplot . t)))
;;**** Gnuplot
(use-package gnuplot)
;;* Theme
;; (use-package monokai-theme
;;   :config
;;   (load-theme 'monokai t))
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-assemblage-theme/")
;; (load-theme "assemblage")

(use-package modus-themes
	:init (modus-themes-load-themes)
	:config (load-theme 'modus-operandi))

;; (use-package badger-theme :ensure t
;; 	:config (load-theme 'badger t))

;; (use-package sublime-themes	:ensure t
;; 	:config (load-theme 'junio t))

;; (use-package gruber-darker-theme
;;    :ensure t
;;    :config (load-theme 'badger t))
;; (use-package birds-of-paradise-plus-theme
;;   :config
;;   (disable-theme 'wheatgrass)
;;   (load-theme 'birds-of-paradise-plus))
(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook (lambda () (interactive) (setq-local show-trailing-whitespace nil)))
;;* Scrolling
(setq scroll-preserve-screen-position t)
;(scroll-bar-mode 1)

(setq ew-scroll-lines-vertical-amount 5)
(setq ew-scroll-lines-horizontal-amount 20)

(global-set-key (kbd "<next>") (lambda () (interactive) (scroll-up ew-scroll-lines-vertical-amount)))
(global-set-key (kbd "<prior>") (lambda () (interactive) (scroll-down ew-scroll-lines-vertical-amount)))
(global-set-key (kbd "C-<next>") (lambda () (interactive) (scroll-left ew-scroll-lines-horizontal-amount)))
(global-set-key (kbd "C-<prior>") (lambda () (interactive) (scroll-right ew-scroll-lines-horizontal-amount)))

;;* Other
;;** Unbind C-_ in undo-tree
(define-key undo-tree-map (kbd "C-_") nil)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)
;;** Resize window bindings
(setq ew-resize-window-lines-horizontal-amount 2)
(setq ew-resize-window-lines-vertical-amount 1)

(global-set-key (kbd "C--") (lambda () (interactive) (shrink-window-horizontally ew-resize-window-lines-horizontal-amount)))
(global-set-key (kbd "C-=") (lambda () (interactive) (enlarge-window-horizontally ew-resize-window-lines-horizontal-amount)))
(global-set-key (kbd "C-_") (lambda () (interactive) (shrink-window ew-resize-window-lines-vertical-amount)))
(global-set-key (kbd "C-+") (lambda () (interactive) (enlarge-window ew-resize-window-lines-vertical-amount)))
;;** Drag text with alt-up/down
(use-package drag-stuff
  :config
  (global-set-key (kbd "M-<up>") (lambda () (interactive) (drag-stuff-up 1)))
  (global-set-key (kbd "M-<down>") (lambda () (interactive) (drag-stuff-down 1))))
;;** Paste with C-v
(define-key key-translation-map (kbd "C-v") (kbd "C-y"))
(define-key key-translation-map (kbd "M-v") (kbd "M-y"))
;;** Enable tab bar
(tab-bar-mode 1)
;;** Disable line wrapping
(set-default 'truncate-lines t)
;;** Enable functions disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
;;** Set default mode
(setq-default major-mode 'prog-mode)
(setq initial-major-mode 'prog-mode)
;;** Bindings to quickly change mode
(global-set-key (kbd "M-p p") 'prog-mode)
(global-set-key (kbd "M-p l") 'lisp-mode)
(global-set-key (kbd "M-p o") 'org-mode)
(global-set-key (kbd "M-p v") 'visual-line-mode)
;; Set tab width
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
;;** Change font size interactively
(defun ew-font-size () (interactive)
	   (let ((size (string-to-number (read-string "New font size: "))))
		 (set-face-attribute 'default nil :height size)))
(global-set-key (kbd "M-p f") 'visual-line-mode)
;;** Convert to title case
(defun xah-title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
  Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

  When called in a elisp program, *begin *end are region boundaries.
  URL `http://xahlee.info/emacs/emacs/elisp_title_case_text.html'
  Version 2017-01-11"
  (interactive
   (if (use-region-p)
	   (list (region-beginning) (region-end))
	 (let (
		   $p1
		   $p2
		   ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
	   (progn
		 (skip-chars-backward $skipChars (line-beginning-position))
		 (setq $p1 (point))
		 (skip-chars-forward $skipChars (line-end-position))
		 (setq $p2 (point)))
	   (list $p1 $p2))))
  (let* (
		 ($strPairs [
					 [" A " " a "]
					 [" And " " and "]
					 [" At " " at "]
					 [" As " " as "]
					 [" By " " by "]
					 [" Be " " be "]
					 [" Into " " into "]
					 [" In " " in "]
					 [" Is " " is "]
					 [" It " " it "]
					 [" For " " for "]
					 [" Of " " of "]
					 [" Or " " or "]
					 [" On " " on "]
					 [" Via " " via "]
					 [" The " " the "]
					 [" That " " that "]
					 [" To " " to "]
					 [" Vs " " vs "]
					 [" With " " with "]
					 [" From " " from "]
					 ["'S " "'s "]
					 ["'T " "'t "]
					 ]))
	(save-excursion
	  (save-restriction
		(narrow-to-region @begin @end)
		(upcase-initials-region (point-min) (point-max))
		(let ((case-fold-search nil))
		  (mapc
		   (lambda ($x)
			 (goto-char (point-min))
			 (while
				 (search-forward (aref $x 0) nil t)
			   (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
		   $strPairs))))))

;;** Dired switches
(custom-set-variables
 '(dired-listing-switches "-alh"))
;;** Nov.el
(use-package nov
	:config
	(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
	(setq nov-text-width 70))
;;** Bind find-file-at-point
(global-set-key (kbd "C-x C-M-f") 'find-file-at-point)
