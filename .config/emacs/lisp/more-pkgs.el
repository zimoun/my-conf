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
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex) ; Dependency to AucTeX because texmathp
  )

(with-eval-after-load 'tex-mode
  (dolist (hook (list 'latex-mode-hook 'LaTeX-mode-hook))
    (add-hook hook 'turn-on-flyspell)))

(with-eval-after-load 'flyspell
  (require 'auto-dictionary)
  (add-hook 'flyspell-mode-hook 'auto-dictionary-mode))

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))

(with-eval-after-load 'info
  (require 'info+))

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
  (require 'guix-prettify)              ; otherwise prettify breaks
  (add-hook 'shell-mode-hook 'guix-prettify-mode)
  (add-hook 'shell-mode-hook 'guix-build-log-minor-mode)
  (add-hook 'dired-mode-hook 'guix-prettify-mode)
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

  ;; Turn on actions in minibuffer: M-o
  (require 'ivy-hydra)

  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-next-line)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  (setq ivy-use-virtual-buffers      t
        ivy-re-builders-alist     '((t . ivy--regex-ignore-order))
        ivy-virtual-abbreviate   'full ; switch-buffer with full path in recentf
        enable-recursive-minibuffers t
        ivy-wrap                     t  ; cycle last->first and first->last
        recentf-max-saved-items    nil
        counsel-find-file-ignore-regexp "\\.go\\'"
        ivy-height 23)

  (custom-set-variables '(counsel-ag-base-command
                          "ag --nocolor --nogroup --hidden %s"))

  ;; Add richer information to the suggestions (docstring, etc.)
  (require 'ivy-richer)
  (ivy-richer-mode))



(with-eval-after-load 'notmuch
  (add-hook 'notmuch-show-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               `((,message-mark-insert-begin
                  . 'font-lock-warning-face)
                 (,message-mark-insert-end
                  . 'font-lock-warning-face)))))

  ;; These hooks should go to their own
  ;; because they are not applied
  ;; if notmuch is not launched before debbug, for example.
  (add-hook 'message-mode-hook 'turn-on-flyspell)
  (add-hook 'message-mode-hook 'typo-mode)

  (custom-set-faces
   '(notmuch-message-summary-face    ((t (:background "dim gray"))))
   '(notmuch-search-matching-authors ((t (:foreground "OliveDrab1"))))
   '(notmuch-search-unread-face      ((t (:weight normal)))))

  (define-key notmuch-show-mode-map (kbd "o") 'browse-url-at-point)

  (defun my/notmuch-search-toogle-deleted ()
    "Toogle +/-deleted tag in `notmuch-search-mode'."
    (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-deleted"))
      (notmuch-search-tag (list "+deleted")))
    (notmuch-search-next-thread))

  (define-key notmuch-search-mode-map (kbd "d")
    'my/notmuch-search-toogle-deleted)

  (defvar my/notmuch-big-query
    " ( tag:unread or tag:to-classify or date:-32d.. or (tag:old  and not tag:flagged  and not thread:{tag:todo} and not thread:{tag:workon} ) )"
    "Complex notmuch search query.")

  ;; c l stashes a hyperlink using Message-ID instead of numbering, e.g.:
  ;; https://yhetil.org/guix-user/acba4413-a4ca-d7e5-08f7-24ac9839b830@posteo.de
  ;; vs https://lists.gnu.org/archive/html/help-guix/2020-10/msg00177.html
  (mapcar (lambda (what)
            (add-to-list 'notmuch-show-stash-mlarchive-link-alist
                         `(,what . ,(concat "https://yhetil.org/" what "/"))))
          (reverse'("guix-devel"
                    "guix-user"
                    "guix-science"
                    "gwl"
                    "guix-bugs"
                    "guix-patches")))

  (setq
   notmuch-show-all-tags-list t
   notmuch-show-indent-messages-width 1
   notmuch-search-oldest-first nil
   notmuch-show-stash-mlarchive-link-default "guix-devel"

   notmuch-search-result-format `(("date"    . "%12s ")
                                  ("count"   . "%-7s ")
                                  ("authors" . "%-20s ")
                                  ("subject" . " %-70s ")
                                  ("tags"    . "(%s)"))

   notmuch-draft-tags '("+draft" "-unread")

   my/notmuch-query-guix-ml
   (concat "tag:guix-ml and " my/notmuch-big-query)
   my/notmuch-query-guix-bug/patch
   (concat "tag:guix-bug/patch and " my/notmuch-big-query)
   my/notmuch-query-lists
   (concat "tag:list and " my/notmuch-big-query)

   notmuch-saved-searches
   `((:name "unread"  :key "u" :query "tag:unread and not tag:delete"
            :sort-order newest-first)
     (:name "to-me"   :key "p" :query "tag:unread and tag:to-me"
            :sort-order newest-first)

     (:name "to-classify"  :key "i" :query "tag:to-classify")
     (:name "workon"  :key "w" :query "tag:workon"
            :search-type tree)
     (:name "todo"    :key "t" :query "tag:todo")

     (:name "guix-ml"        :key "l"
            :query ,my/notmuch-query-guix-ml)
     (:name "guix-bug/patch" :key "b"
            :query ,my/notmuch-query-guix-bug/patch)
     (:name "lists"          :key "L"
            :query ,my/notmuch-query-lists)

     (:name "scirep"   :key "as" :query "tag:scirep")
     (:name "misc-bio" :key "ab" :query "tag:misc-bio")

     (:name "old"     :key "O" :query "tag:old")
     (:name "flagged" :key "f" :query "tag:flagged")
     (:name "me"      :key "m"   :query "tag:sent or tag:to-me"
            :sort-order newest-first)
     (:name "draft"      :key "D"   :query "tag:draft"
            :sort-order newest-first))

   notmuch-tagging-keys
   `((,(kbd "i")  ("+to-classify") "to-classify")
     (,(kbd "t")  ("+todo")         "todo")
     (,(kbd "w")  ("+workon")       "workon")
     (,(kbd "f")  ("+flagged")      "Flag/starred")
     (,(kbd "as") ("+scirep")       "scirep")
     (,(kbd "ab") ("+misc-bio")     "misc-bio")
     (,(kbd "d")  ("+deleted" "-inbox" "-unread" "-flagged") "Delete"))))


(provide 'more-pkgs)

;;; more-pkgs.el ends here
