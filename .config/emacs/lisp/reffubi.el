;;; set-ibuffer -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(with-eval-after-load 'ibuffer
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Run" (or
                         (name . "^\*eshell.*\*$")
                         (mode . shell-mode)
                         (name . "^\\*compilation\\*$")))
                 ("C/C++" (or
                           (mode . c-mode)
                           (mode . c++-mode)))
                 ("H/Hpp" (or
                           (mode . c-mode)
                           (mode . c++-mode)))
                 ("Haskell" (mode . haskell-mode))
                 ("caML" (mode . tuareg-mode))
                 ("ELisp" (mode . emacs-lisp-mode))
                 ("Scheme" (mode . scheme-mode))
                 ("Lisp" (mode . lisp-mode))
                 ("(La)TeX(info)" (or
                                   (mode . tex-mode)
                                   (mode . latex-mode)
                                   (mode . texinfo-mode)
                                   (name . "^\\*tex-shell\\*$")))
                 ("Py" (or
                        (mode . python-mode)
                        (name . "^\\*Python.*\*$")))
                 ("ESS[R/jl]" (or
                               (mode . ess-mode)
                               (mode . ess-r-mode)
                               (mode . ess-julia-mode)
                               (name . "^\*R.*\*$"))) ;FIXME: buffer *Reviving Emacs* falls here
                                                      ; even if notmuch-show-mode
                 ("Org" (and
                         (mode . org-mode)
                         (not (name . "^extra-log.org$"))
                         (not (name . "^future.org$"))
                         (not (name . "^todo.org$"))
                         (not (name . "^work.org$"))))
                 ("Todo &co"
                  (or
                   (name . "^extra-log.org$")
                   (name . "^future.org$")
                   (name . "^todo.org$")
                   (name . "^work.org$")
                   (name . "^z-colombbus.org$")))
                 ("PDF" (name . "^[a-zA-Z0-9. -_]*\.pdf$"))
                 ("Dired" (mode . dired-mode))
                 ("Magit" (mode . magit-status-mode))
                 ("Misc magit" (or
                                (name . "\*magit")
                                (name . "^magit")
                                (mode . diff)))
                 ("Notmuch" (or
                             (mode . notmuch-hello-mode)
                             (mode . notmuch-search-mode)
                             (mode . notmuch-show-mode)
                             (mode . notmuch-tree-mode)
                             (mode . notmuch-message-mode)))
                 ("Debbugs" (or
                             (mode . debbugs-gnu-mode)
                             (mode . gnus-summary-mode)
                             (mode . gnus-article-mode)
                             (mode . message-mode)))
                 ("IRC" (mode . erc-mode))
                 ("Info" (or
                          (mode . Info-mode)
                          (mode . help-mode)))
                 ("Emacs" (or
                           (name . "^\\*[a-zA-Z+:#0-9 -/]*\\*$")
                           ))

                 ;; Match any string not containing any uppercase letter
                 ;; ("lower" (name . "\\`[^[:upper:]]*\\'"))
                 ;; Not sure what is doing
                 ;; ("Upper" (name . "[[:upper:]]"))
                 )))
        ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'alphabetic)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (setq-local case-fold-search nil)
              (ibuffer-switch-to-saved-filter-groups "default"))))


(provide 'reffubi)

;;; reffubi.el ends here

