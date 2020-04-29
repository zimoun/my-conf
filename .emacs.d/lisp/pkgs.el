;;; the-packages-config -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'guix-prettify-mode)

  (define-key dired-mode-map (kbd "r") 'my/dired-sort)
  (define-key dired-mode-map (kbd "=") 'my/dired-ediff-or-diff)
  (define-key dired-mode-map (kbd "E") 'dired-toggle-read-only)

  (setq dired-listing-switches "-alh"
        dired-recursive-deletes 'always)

  ;; the confirmation style is hard coded
  (defalias 'dired--yes-no-all-quit-help 'y-or-n-p)

  (defun my/dired-ediff-or-diff (universal)
    "Improve diffing using `dired'.

Improve Info node `(emacs) Dired' mode by adding `ediff-files' to
the keybinding `=' through UNIVERSAL argument.

`=' binds `dired-diff' at point.

`C-u =' binds `ediff-files':
 - if no file is marked: use file at point and ask other file;
 - if one file is marked: use other file at point;
 - if two files are marked: use them."
    ;; "P" to provide `current-prefix-arg' as UNIVERSAL argument
    (interactive "P")

    (let ((files (dired-get-marked-files))
          file1 file2)

      (if (> (length files) 2)
          (error (format
                  "Error: %d marked files, instead of 2. %s"
                  (length files) (mapcar 'file-name-nondirectory files)))

        (if (eq universal nil)
            (progn
              (setq file1 (dired-get-filename t))
              (setq file2
                    (read-file-name
                     (format "[Diff] File B to compare (%s) : "
                             (file-name-nondirectory file1))
                     ;; (dired-dwim-target-directory))))
                     (dired-current-directory)))
              (dired-diff file2))

          (progn
            (when (= (length files) 1)
              (progn
                (setq file1 (car files))

                (if (string= file1 (expand-file-name (dired-get-filename t)))
                    (setq file2
                          (read-file-name
                           (format "[Ediff] File B to compare (%s) : "
                                   (file-name-nondirectory file1))
                           ;; (dired-dwim-target-directory))))
                           (dired-current-directory)))
                  (setq file2 (dired-get-filename t)))
                ))

            (when (= (length files) 2)
              (progn
                (setq file1 (car files))
                (setq file2 (cadr files))))

            (ediff-files file1 file2)))))))


(with-eval-after-load 'comint-mode
  ;; Redefine M-p/M-n because old habits
  (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<down>") 'comint-next-input))


(with-eval-after-load 'whitespace
  (setq whitespace-line-column nil
        whitespace-style '(face trailing lines-tail
                                space-before-tab newline
                                indentation empty space-after-tab)))

(with-eval-after-load 'ediff
  ;; Nicer with tiling
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))


(with-eval-after-load 'erc
  (setq
   erc-nick "zimoun"
   erc-join-channels-alist '(("freenode.net" "#guix"))))


(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"))


(with-eval-after-load 'ispell
  (setq ispell-program-name "aspell"))


(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda ()
                (add-hook 'after-save-hook
                          (lambda ()
                            (let ((compiled-buffer (concat buffer-file-name "c")))
                              (when (file-exists-p compiled-buffer)
                                (delete-file compiled-buffer))))
                          nil t)        ; Append at the beginning (nil)
                                        ; Buffer local value (t)
                ))
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))


(with-eval-after-load 'debbugs-gnu
  ;; deactivate Ivy for the function `debbugs-gnu-search'
  ;; Enter attribute indefinitively loops
  ;; because Ivy always suggests a completion
  ;; then it is impossible to enter an empty key
  (add-hook 'debbugs-gnu-mode-hook #'(lambda ()
                                       (setq-local completing-read-function
                                                   #'completing-read-default)))

  (define-key debbugs-gnu-mode-map "N" 'debbugs-gnu-narrow-to-status)
  (define-key debbugs-gnu-mode-map "/" 'debbugs-gnu-search)
  (define-key debbugs-gnu-mode-map "#" 'debbugs-gnu-bugs)

  (setq debbugs-gnu-default-packages '("guix-patches" "guix"))
  (add-to-list 'debbugs-gnu-all-packages "guix-patches")

  ;; inspired by Oleg Pykhalov from mailing list
  (defun my/debbugs-query-email (email-address)
    "List all the bug report that EMAIL-ADDRESS has opened."
    (interactive
     (let* ((default "zimon.toutoune@gmail.com")
            (string (read-string (format "Email address (%s): " default))))
       (when (not (equal string ""))
         (setq default string))
       (list default)))
    (let ((debbugs-gnu-current-query `((submitter . ,email-address))))
      (debbugs-gnu nil))
    (message (format "Debbugs current-query: submitter=%s" email-address))))


(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "C-d")
                'my/comint-delchar-or-maybe-eof)
              (define-key shell-mode-map (kbd "C-r") 'counsel-shell-history)
              ))

  ;; Redefine M-p/M-n because old habits
  (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key shell-mode-map (kbd "<down>") 'comint-next-input)

  (defun my/comint-delchar-or-maybe-eof (arg)
    "See `comint-delchar-or-maybe-eof' with `kill-current-buffer'."
    (interactive "p")
    (let ((proc (get-buffer-process (current-buffer))))
      (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
          (progn
            (comint-send-eof)
            (kill-current-buffer)
            )
        (delete-char arg))))

  (defun my/shell ()
    "Open new shell with new buffer name."
    (interactive)
    (shell (generate-new-buffer-name "*shell*"))))



(with-eval-after-load 'cc-vars
  (setf (cdr (assoc 'other c-default-style)) "linux")
  (add-hook 'c-mode-common-hook 'whitespace-mode)
  (add-hook 'c-mode-common-hook 'dtrt-indent-mode)
  (add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))
  (add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'google-set-c-style))


(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'eldoc-mode)
  (add-hook 'python-mode-hook 'my/return-newline-with-indent)

  (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)

  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))


(with-eval-after-load 'tex-mode
  (add-hook 'latex-mode-hook 'my/return-newline-with-indent)
  (add-hook 'latex-mode-hook 'turn-on-auto-fill)
  (add-hook 'latex-mode-hook 'turn-on-reftex)

  ;; be careful, not latex-mode-hook but tex-mode-hook !!
  (add-hook 'tex-mode-hook
            (lambda ()
              (define-key tex-mode-map (kbd "C-c C-r") 'my/compile-or-recompile)
              (define-key tex-mode-map (kbd "C-c C-S-R") 'tex-region)))

  (setq revert-without-query '(".+pdf$"))
  (setq reftex-ref-macro-prompt nil))


(setq org-enforce-todo-dependencies t)	; Need to be initialized before Org is loaded
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex) ; Dependency to AucTeX because texmathp

  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  (add-hook 'org-mode-hook 'org-babel-result-hide-all)

  (define-key org-mode-map (kbd "C-c l")  'org-store-link)

  (setq org-special-ctrl-a/e t)		; More "intuitive" beginning of line

  ;; My prefered and used backends
  ;;;; need to be set up before loading org.el
  ;; (or not, depending on Emacs's version ?)
  (setq org-export-backends '(ascii html latex texinfo))

  (setq org-agenda-files (list
                          "~/org/"
                          ))
  (setq org-agenda-include-diary nil)

  (setq org-tag-faces
        '(
          ;;("@meet" . (:foreground "Chartreuse4" :weight bold :underline t))
          ("@meet" . (:foreground "mediumseagreen" :weight bold :underline t))
          ("URGENT" . (:foreground "Red" :underline t))
          ))

  (put 'narrow-to-region 'disabled nil)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (R . t)
                               (C . t)
                               (shell . t)
                               (org . t)
                               (makefile . t)
                               (scheme . t)
                               ))
                                        ;(bash . t)))
  ;; do not ask before eval code blocks
  (setq org-confirm-babel-evaluate nil)
  ;; In org-mode 9 you need to have #+PROPERTY: header-args :eval never-export
  ;; in the beginning or your document to tell org-mode not to evaluate every
  ;; code block every time you export.

  ;; C-c ' do not indent when leaves
                                        ;(setq org-edit-src-content-indentation 0)

  ;; store time when TODO is DONE
  (setq org-log-done (quote time))

  (setq org-src-fontify-natively t)     ; coloring   inside blocks
  (setq org-src-tab-acts-natively t)	; completion inside blocks)
  (setq org-src-window-setup 'current-window)
  ;; (setq org-hide-emphasis-markers t)    ; hide the *,=, or / markers
  (setq org-cycle-separator-lines 1)    ; number of empty lines
                                        ; needed to keep empty
                                        ; between collapsed trees

  ;; Follow internal link C-c C-l
  ;;;; file:stuff.org::Key1 key2
  ;;;; then C-c C-o open the link searching with the keywords Key1 key2
  ;;;; The search is fuzzy. Otherwise, by default 'query-replace-to it is strict.
  (setq org-link-search-must-match-exact-headline nil)


  ;; Add notes (C-c C-z) in LOGBOOK
  (setq org-log-into-drawer t)

  ;; Set quick capture
  (defalias 'orgadd 'org-capture)
  (setq org-capture-templates
        (quote
         (("t" "Todo")
          ("tt" "TODO entry" entry
           (file+headline "~/org/todo.org" "Capture")
           (file "~/.emacs.d/org-templates/todo.org"))
          ("tm" "Misc and URGENT" entry
           (file+headline "~/org/todo.org" "Misc")
           (file "~/.emacs.d/org-templates/urgent.org"))
          ("d" "Diary" entry
           (file+headline "~/org/diary.org" "Capture")
           (file "~/.emacs.d/org-templates/done.org"))
          ("b" "Bookmark" entry
           (file+headline "~/org/bookmarks.org" "Bookmarks")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1))
         ))

  (if (not (version-list-< '(9 2) (version-to-list org-version)))
      (progn
        (warn "Please update Org-mode.\ne.g., by installing `org-plus-contrib'.\nKeybindings <s need `(require 'org-tempo)' to work again.")
        ;; Try <el TAB
        (add-to-list 'org-structure-template-alist
                     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")))
    (progn
      ;; With 9.2 the keybindings <s does not work anymore
      ;; The Org Tempo allow the previous mechanism
      ;; https://orgmode.org/Changes.html#org1b5e967
      (require 'org-tempo)
      ;; see org-structure-template-alist
      (add-to-list 'org-structure-template-alist
                   '("sel" . "src emacs-lisp")))
    )

  ;; Add the support of LaTeX macro when exporting (pdf or html)
  ;;;; Write your LaTeX macros in source block latex-macro1
  (add-to-list 'org-src-lang-modes '("latex-macro" . latex))

  (defvar org-babel-default-header-args:latex-macro
    '((:results . "raw drawer")
      (:exports . "results")))

  (defun org-babel-execute:latex-macro (body _params)
    (defun prefix-all-lines (pre body)
      (with-temp-buffer
        (insert body)
        (string-insert-rectangle (point-min) (point-max) pre)
        (buffer-string)))
    (concat
     (prefix-all-lines "#+LATEX_HEADER: " body)
     "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
     (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
     "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))


  (defun org-latex-export-as-latex-only ()
    "How to customize `org-export-dispatch'?"
    (interactive)
    (org-latex-export-as-latex nil nil nil t nil))

  (defun org-latex-export-to-latex-only ()
    "How to customize `org-export-dispatch'?"
    (interactive)
    (org-latex-export-to-latex nil nil nil t nil))
  )


(provide 'pkgs)

;;; pkgs.el ends here
