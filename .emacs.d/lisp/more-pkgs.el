;;; more-packages -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'diminish)

(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.jl$" . ess-julia-mode))

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(with-eval-after-load 'auto-revert-mode
  (diminish 'auto-revert-mode))

(with-eval-after-load 'whitespace-mode
  (diminish 'whitespace-mode "WhSp"))

(with-eval-after-load 'org
  (diminish 'org-cdlatex-mode)
  (add-hook 'org-mode-hook 'flyspell-mode))

(with-eval-after-load 'tex-mode
  (add-hook 'tex-mode-hook 'flyspell-mode))

(with-eval-after-load 'flyspell
  (require 'auto-dictionary)
  (add-hook 'flyspell-mode-hook 'auto-dictionary-mode))

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))

(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook 'whitespace-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'guix-devel-mode)
  (add-hook 'scheme-mode-hook 'yas-minor-mode))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "RET") 'my/ilectrify-return-if-match)
  (diminish 'paredit-mode "ParEd"))

(with-eval-after-load 'flycheck
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(with-eval-after-load 'ag
  (setq ag-highlight-search t)
  (custom-set-variables '(ag-arguments '("--smart-case"
                                         "--stats"
                                         "--hidden"
                                         "--skip-vcs-ignores"
                                         "--follow"
                                         "--silent"))))

(with-eval-after-load 'magit
  (add-hook 'magit-status-sections-hook 'magit-insert-recent-commits)
  (setq magit-view-git-manual-method 'woman)

  ;; Ugly hack! Not sure it is the right way...
  ;;;; but seems do the job.
  (defun my/magit-initially-hide-untracked (section)
    "Used by `magit-section-set-visibility-hook'.

See Info node `(magit)Section Visibility'.

From URL `https://emacs.stackexchange.com/questions/20754/change-the-default-visibility-of-a-magit-section'. "
    (and (not magit-insert-section--oldroot)
         (eq (magit-section-type section) 'untracked)
         'hide)))

(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/src/guix/guix/etc/snippets")
  (yas-reload-all)
  (diminish 'yas-minor-mode))

(with-eval-after-load 'ws-butler
  (diminish 'ws-butler-mode))

(with-eval-after-load 'page-break-lines
  (diminish 'page-break-lines-mode))

(with-eval-after-load 'geiser-impl
  (setq geiser-active-implementations '(guile)))

(with-eval-after-load 'guix
  (add-hook 'shell-mode-hook 'guix-prettify-mode)
  (add-hook 'shell-mode-hook 'guix-build-log-minor-mode)
  (add-to-list 'guix-prettify-special-modes 'help-mode))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (setq markdown-open-command "marked"))

(with-eval-after-load 'haskell-mode
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)

  ;; allow to cycle by TAB
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  ;; not sure if it is useful?
  (setq haskell-indentation-electric-flag t)
  ;; apparently not really maintained.
  ;; however useful haskell-indent-align-guards-and-rhs etc.
  (add-hook 'haskell-mode-hook 'haskell-indent-mode)
  ;; provide e.g., nice beginning-of-defun binded to C-M-a
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

  (define-key haskell-mode-map (kbd "C-c C-p") 'haskell-interactive-bring))

(with-eval-after-load 'ess
  (require 'ess-julia)
  (require 'ess-utils)

  (add-hook 'R-mode-hook
            (lambda ()
              (progn
                (ess-set-style 'RStudio)
                (setq ess-indent-offset tab-width)

                ;; Not sure this is still useful
                ;; remap (default) "_" to "<-" by "=" to "<-"
                (setq ess-smart-S-assign-key "=")
                ;; needs to double `(ess-toggle-S-assign nil)'
                (ess-toggle-S-assign nil)
                (ess-toggle-S-assign nil)

                ;; Fix the assign
                (local-set-key (kbd "=") 'ess-cycle-assign)
                )))
  (setq ess-assign-list '(" <- " " = " " <<- " " -> " " ->> "))
  (setq ess-eval-visibly-p nil)
  (setq ess-use-eldoc'script-only))


(with-eval-after-load 'ivy
  (diminish 'ivy-mode)

  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-next-line)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  (setq ivy-use-virtual-buffers      t
        ivy-re-builders-alist     '((t . ivy--regex-ignore-order))
        ivy-virtual-abbreviate   'full ; switch-buffer with full path in recentf
        enable-recursive-minibuffers t
        ivy-wrap                     t  ; cycle last->first and first->last
        recentf-max-saved-items    nil
        ivy-height 23)

  (custom-set-variables '(counsel-ag-base-command
                          "ag --nocolor --nogroup --hidden %s"))

  ;; Add richer information to the suggestions (docstring, etc.)
  (require 'ivy-richer)
  (ivy-richer-mode))




(with-eval-after-load 'notmuch

  )


(provide 'more-pkgs)

;;; more-pkgs.el ends here
