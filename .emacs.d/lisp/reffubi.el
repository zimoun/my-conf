;;; set-ibuffer -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(with-eval-after-load 'ibuffer
  (setq ibuffer-saved-filter-groups
        (quote (("default"
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
                               (name . "^\*R.*\*$")))
                 ("Org" (mode . org-mode))
                 ("PDF" (name . "^[a-zA-Z0-9. ]*\.pdf$"))
                 ("Magit" (or
                           (name . "\*magit")
                           (name . "^magit")))
                 ("Dired" (mode . dired-mode))
                 ("Notmuch" (or
                             (mode . notmuch-hello-mode)
                             (mode . notmuch-search-mode)
                             (mode . notmuch-show-mode)
                             (mode . notmuch-tree-mode)))
                 ("Message" (mode . message-mode))
                 ("Debbugs" (or
                             (mode . debbugs-gnu-mode)
                             (mode . gnus-summary-mode)
                             (mode . gnus-article-mode)))
                 ("Run" (or
                         (name . "^\*eshell.*\*$")
                         (mode . shell-mode)
                         (name . "^\\*compilation\\*$")))
                 ("Info" (or
                          (mode . Info-mode)
                          (mode . help-mode)))
                 ("emacs" (or
                           (name . "^\\*[a-zA-Z+:#0-9 -]*\\*$")
                           (mode . debbugs-gnu-mode)
                           (mode . erc-mode)))

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

