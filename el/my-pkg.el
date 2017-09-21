
(provide 'my-pkg)

;; numbering lines (and add column in mode-line)
(use-package linum
  :init
  (global-linum-mode 1)
  (setq linum-format "%d ")
  ;; to display the number of the column
  (column-number-mode t)
  :config
  (defcustom linum-disabled-modes-list '(
                                         compilation-mode
                                         dired-mode
                                         doc-view-mode
                                         tex-(should )ell-mode
                                         )
    "* List of modes disabled when global linum mode is on"
    :type '(repeat (sexp :tag "Major mode"))
    :tag " Major modes where linum is disabled: "
    :group 'linum
    )
)

;; highlight parens
(use-package paren
  :config
  (show-paren-mode 1)
  ;; not sure it is the right place here
  (electric-pair-mode 1)
)

;; ;; easy Lisp manipulation with paredit
;; (use-package paredit
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;;   ;; (add-hook 'emacs-lisp-mode
;;   ;;           (lambda ()
;;   ;;             (define-key emacs-lisp-mode-map
;;   ;;               (kdb "C-k") 'my/paredit-kill)
;;   ;;             ))
;;   :bind ("C-k" . my/paredit-kill)
;;   ;; :diminish paredit-mode
;;   )


;; save history
(use-package savehist
  :config
  (savehist-mode 1)
)

;; easy switch between buffers
(use-package ido
  :defer t
  :init (ido-mode 'buffers)
  :config
  (defun ido-ignore-except (name)
    "Ignore all non-user buffer (i.e., *name*) with exceptions

      except:  *eshell* etc."
    ;; (and (string-match "^\*" name)
    ;;      (not (string-match  "^\*eshell.*\*$" name))
    ;;      (not (string-match  "^\*Python.*\*$" name))
    ;;      (not (string-match "^\*Inferior Octave\*$")))
  )
  ;(setq ido-ignore-buffers '("\\` " ido-ignore-except))
  ;; ido-switch-buffer implies problem ?
  ;; 1/ emacsclient -c foo -> frame1
  ;; 2/ emacsclient -c bar -> frame2
  ;; 3/ switch to foo in frame2 ==> nope !
;;;; this line fixes the problem
  (setq ido-default-buffer-method 'selected-window)
)

;; the nice buffer management
(use-package ibuffer
  :defer t
  :init
  ;; C-x C-b ibuffer as default
  (defalias 'list-buffers 'ibuffer)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           ;; (name . "^\\*Messages\\*$")
                           ;; (name . "^\\*Help\\*$")
                           ;; (name . "^\\*Completions\\*$")
                           ;; (name . "^\\*Calculator\\*$")
                           ;; (name . "^\\*Calendar\\*$")
                           ;; (name . "^\\*Calc Trail\\*$")
                           ;; (name . "^\\*Compile-Log\\*$")
                           (name . "^\\*tramp.*\\*$")
                           ))
                 ;; ("magit" (name . "^\\*magit.[-_:a-zA-Z0-9 ]+$"))
                 ("magit" (name . "\*magit"))
                 ("Run" (or
                         ;; (name . "^\*eshell\*$")
                         (name . "^\*eshell.*\*$")
                         (name . "^\\*compilation\\*$")
                         (name . "^\\*tex-shell\\*$")
                         (name . "^\\*Python.*\*$")
                         (name . "^\*Inferior Octave\*$")
                         ))
                 ("Py" (mode . python-mode))
                 ("C/C++" (or
                           (mode . c-mode)
                           (mode . c++-mode)
                           ;; (name . "\\.c$")
                           ))
                 ("H/Hpp" (or
                           (mode . c-mode)
                           (mode . c++-mode)
                           ))
                 ("caML" (mode . tuareg-mode))
                 ("Lisp" (or
                          (mode . lisp-mode)
                          (mode . emacs-lisp-mode)
                          (mode . scheme-mode)
                          ))
                 ("(La)TeX" (or
                             (mode . tex-mode)
                             (mode . latex-mode)
                             ))
                 ("ESS[R]" (mode . ess-mode))

                 ("lower" (name . "\\`[^[:upper:]]*\\'"))
                 ("Upper" (name . "[[:upper:]]"))
  ))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (setq-local case-fold-search nil)
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-default-sorting-mode 'alphabetic)
  (setq ibuffer-show-empty-filter-groups nil)
)

;; files manager
(use-package dired
  :defer t
  :init (defalias 'list-directory 'dired)
  :config
  ;;(setq dired-listing-switches "-alr")
  (define-key dired-mode-map (kbd "r") 'my/dired-sort)
  (define-key dired-mode-map (kbd "=") 'my/dired-ediff-or-diff)
)

;; Emacs shell
(use-package eshell
  :defer t
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)
    (setq eshell-where-to-jump 'begin)
    (setq eshell-review-quick-commands nil)
  )
)

;; Bookmark facilities
(use-package bookmark
  :defer t
  ;; :init
  ;; (bookmark-bmenu-list)
  ;; (switch-to-buffer "*Bookmark List*")
  :config
  (setq bookmark-save-flag t)
  (setq bookmark-version-control t)
  )

(use-package diminish
  :ensure t
  :defer t
  )

;; show function signature in Mini-Buffer
(use-package eldoc
  :defer t
  :diminish t
  :config
  (diminish 'eldoc-mode)
  )

;; ;;
;; ;; Hum? not sure it works as I want.
;; ;; And it adds possibly long stuff in the (short) mode line.
;; ;;
;; ;; display the current function in mode line
;; (use-package which-func
;;   ;; which-func-mode is deprecated
;;   ;; but which-function-mode is defined in which-func.el
;;   :init
;;   (which-function-mode t)
;;   (setq which-func-unknown ".")
;;   (setq which-func-modes
;;         '(python-mode
;;           lisp-mode
;;           julia-mode
;;           ess-mode
;;           cc-mode
;;           ))
;; )

;; ELisp
(use-package lisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'my/save-file-and-remove-filec)
)

;; remote editing
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory "~/.emacs.d/auto-save-list")
)

;; ;; catch all the keys
;; (use-package keyfreq
;;   :ensure t
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1)
;;   (setq keyfreq-file "~/.emacs.d/emacs-keyfreq")
;;   (setq keyfreq-autosave-timeout 1)
;; )

;; C/C++ stuffs
(use-package cc-mode
  :defer t
  :config
  (setq c++-default-style "linux")
  (setq c++-basic-offset 4)
  ;; c++/c should be merged ? (with mode-common?)
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook 'my/return-newline-with-indent)
)

;; python (which mode ?)
(use-package python
  :ensure t
  :defer t
  :mode ("\\.pyx$" . python)
  ;; hum? not sure to understand init/config mechanism
  ;; if I understand well,
  ;; the first hook is applied at startup time
  ;; the second hook is applied when python is required.
  :init
  (add-hook 'python-mode-hook 'eldoc-mode)
  :config
  (add-hook 'python-mode-hook 'my/return-newline-with-indent)
  ;; Need to define function something in this flavor
  ;; in order to create the TAGS file
  ;; Should run etags even in the imported lib
  ;; workon foo
  ;; find ${$(which python)/\/bin\/python/} -type f -name '*.py' | xargs etags
)


;; LaTeX
(use-package tex-mode
  :defer t
  :config
  (add-hook 'latex-mode-hook 'my/return-newline-with-indent)
  (setq revert-without-query '(".+pdf$"))
  ;; Latex hook
  ;; be careful, not latex-mode-hook but tex-mode-hook !!
  (add-hook 'tex-mode-hook
          (lambda ()
            (define-key tex-mode-map (kbd "C-c C-r") 'my/compile-or-recompile)
            (define-key tex-mode-map (kbd "C-c C-S-R") 'tex-region)
            )
          )
  ;; the so nice RefTeX package...
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (setq reftex-ref-macro-prompt nil)
)

;; mispell corrector using dictionary
(use-package ispell
  :defer t
  :config
  (setq-default ispell-program-name "aspell")
)

;; OCalm improved mode
(use-package utop
  :ensure t
  :defer t)
(use-package tuareg
  :ensure t
  :defer t
  :mode ("\\.ml[ily]?$" . tuareg-mode)
  :bind (:map tuareg-mode-map
         ("C-c C-e" . utop-eval-phrase))
  :init
  (defalias 'run-ocaml 'utop)
  :config
  (autoload 'utop "utop" "Toplevel for Ocaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop/ocaml" t)
  ;; (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for Ocaml" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
)

;; Lua used by Awesome
(use-package lua-mode
  :ensure t
  :defer t
  :mode ("\\.lua$" . lua-mode)
)

;; octave and so on
(use-package octave
  :ensure t
  :defer t
  :mode ("\\.m\\'" . octave-mode)
  :config
  (setq octave-mode-hook
      (lambda () (progn (setq octave-comment-char ?%)
                        (setq comment-start "% ")
                        (setq comment-add 0))))
)
;; Editing blog
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'")
  :config
  (progn
    (add-hook 'markdown-mode-hook 'auto-fill-mode)
    (setq markdown-open-command "marked")
  )
)
;; see if useful ?
(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  ;; (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
)


;; not really used (yet)
(use-package org
  :defer t
  :config
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-agenda-files (list "/tmp/cal.org"))
  (put 'narrow-to-region 'disabled nil)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t) (R . t)))

  (setq org-agenda-include-diary t)
  (setq org-log-done (quote time))

  (setq org-src-fontify-natively t)
)

;; graphviz support
(use-package graphviz-dot-mode
  :ensure t
  :defer t
)

;; (use-package julia-mode
;;   :ensure t
;;   :defer t
;;   :mode ("\\.jl$" . julia-mode)
;;   ;; :config
;;   ;; ;(require 'ess-site)
;;   ;; (setq inferior-ess-julia-program-name "julia")
;; )

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
)

(use-package eshell
  :ensure t
  :defer t
  :config
  (setq eshell-prompt-function
        (lambda nil
          (concat
           "\n"
           (replace-regexp-in-string
            (getenv "HOME") "~" (eshell/pwd))
           "\n $ "
           )
          ;;"\n"(user-login-name) "@" (system-name) " $ ")
        )
   )
  (use-package esh-toggle
    :defer t
    :bind ("C-x C-z" . eshell-toggle)
  )
)

(use-package yasnippet
  :ensure t
  :defer t
  :diminish t
  :init
  (yas-global-mode 1)
  :config
  (yas-global-mode 1)
  (diminish 'yas-minor-mode)
  )

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable))

(use-package pyvenv
  :ensure t
  :defer t
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --profile=ipy --simple-prompt")
  ;; Python and conda env
  (setenv "WORKON_HOME" "~/miniconda2/envs")
  (pyvenv-mode 1)
)

(use-package ess
  :defer t
  :ensure t
  :init
  :defines ess-indent-offset
  :mode (
         ("\\.[r|R]\\'" . R-mode)
         ("\\.jl\\'" . ess-julia-mode)
         )
  :config
  (require 'ess-site)
  (require 'ess-utils)
  (require 'ess-rutils)
  (add-hook 'R-mode-hook
            (lambda ()
              (ess-set-style 'RStudio)
              (setq ess-indent-offset tab-width)
              ;; remap (default) "_" to "<-" by "=" to "<-"
              (setq ess-smart-S-assign-key "=")
              ;; needs to double `(ess-toggle-S-assign nil)'
              (ess-toggle-S-assign nil)
              (ess-toggle-S-assign nil)
              ))
  (setq ess-eval-visibly-p nil)
  (setq ess-use-eldoc'script-only)
  )

;; (use-package ess-site
;;   :ensure ess
;;   :mode (
;;          ("\\.R\\'" . R-mode)
;;          ;("\\.jl\\'" . julia-mode)
;;          )
;;   :defines ess-indent-offset
;;   :config
;;   (add-hook 'R-mode-hook
;;             (lambda ()
;;               (setq ess-indent-offset tab-width)))
;;   (require 'ess-rutils)
;;   (setq ess-eval-visibly-p nil)
;;   ;; change "_" to "<-" by "=" to "<-"
;;   ;; the double `(ess-toggle-S-assign nil)'
;;   (setq ess-smart-S-assign-key "=")
;;   (ess-toggle-S-assign nil)
;;   (ess-toggle-S-assign nil)
;;   ;;(setq ess-indent-level 2)
;;   (ess-set-style 'RStudio)
;;   ;;(setq ess-use-eldoc t)
;;   (setq ess-use-eldoc'script-only)
;; )


(use-package geiser
  :ensure t
  :defer t
  :init
  ;; scheme
  (setq scheme-program-name "guile")
  ;;(setq geiser-default-implementation 'guile)
  ;;(setq geiser-default-implementation '(guile))
  :config
  (add-hook 'scheme-mode-hook 'geiser-mode)
  ;; (setq geiser-default-implementation '(guile))
  ;; (setq geiser-implementation-alist '(((regexp "\\.scm$") guile)))
  (setq geiser-active-implementations '(guile))
)

;; (use-package bibtex-completion
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq bibtex-completion-bibliography
;;         '("~/tmp/bibjabref.bib"))
;;   )

(use-package helm-bibtex
  :ensure t
  :defer t
  :config
  (setq bibtex-completion-bibliography
        '("~/bib/bibjabref.bib"))
  (setq bibtex-completion-library-path
        (mapcar (lambda (x) (concat "~/bib/pdf/" x))
                '("article/"
                "book/"
                "CO/"
                "conf/"
                "report/"
                "TH/")))
  ;; useful for `helm-find-file': tab does completion
  ;; but not for `helm-M-x': tab open stuff and does not complete
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<backtab>") 'helm-select-action)
  )


(use-package haskell-mode
  :ensure t
  :defer t
  :config
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

  (define-key haskell-mode-map (kbd "C-c C-p") 'haskell-interactive-bring)
)
