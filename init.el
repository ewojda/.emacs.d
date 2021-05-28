(package-initialize)
(load "~/.emacs.d/sanemacs.el" nil t)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

(defun config-initialize ()
;; TODO
  )

;; Re-enable menu-bar disabled by sanemacs.el
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode 1))

;; Change theme
(if (not custom-enabled-themes)
    (load-theme 'wheatgrass t))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;;(require 'company-lsp)
;;(require 'lsp-ui)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\M-up] 'move-text-up)
(global-set-key [\M-down] 'move-text-down)
















;; Smieci ------------------------------------------------------
;; (setq office-replace-macro-1-enter_edit
;; ;;       [escape ?\C-s ?\\ ?. ?o ?d ?\[ ?s ?t ?\] return return ?\C-c ?\C-c ?\C-s ?c ?o ?n ?t ?e ?n ?t ?\\ ?. backspace backspace ?. ?x ?m ?l return return] )

;; ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r tab tab ?g ?e tab return ?2 ?0 ?2 ?0 return ?2 ?0 ?2 ?1 return
;; (setq office-replace-macro-2-save
;;       [?\C-x ?\C-s])

;; (setq office-replace-macro-3-close
;;       [?\C-x ?k return ?\C-x ?k return])

;; (defun office-replace (match replace)
;;   (execute-kbd-macro office-replace-macro-1-enter_edit)
;;   (replace-regexp match replace)
;;   (ignore-errors
;;     (execute-kbd-macro office-replace-macro-2-save))
;;   (execute-kbd-macro office-replace-macro-3-close))
;; (defun office-replace-a (match replace)
;;   (execute-kbd-macro [return ?\C-c ?\C-c])
;;   (search-forward "content.xml")
;;   (execute-kbd-macro [return])
;;   (ignore-errors
;;     (replace-regexp match replace))
;;   (save-buffer)
;; )
;; (defun office-replace (match replace)
;;   (ignore-errors
;;     (office-replace-a match replace))
;;   (execute-kbd-macro [?\C-x ?\C-k ?\C-x ?\C-k])
;; )
;; Koniec smieci --------------------------------------------
