;;; the-packages-config -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'guix-prettify-mode)

  (setq dired-listing-switches "-alh"
        dired-recursive-deletes 'always))


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

  (setq
   debbugs-gnu-default-packages '("guix-patches" "guix")
   gnus-summary-line-format "%I%(%[ %n%]%) %s\n")
  (add-to-list 'debbugs-gnu-all-packages "guix-patches"))


(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "C-r") 'counsel-shell-history)))

  ;; Redefine M-p/M-n because old habits
  (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key shell-mode-map (kbd "<down>") 'comint-next-input))


(with-eval-after-load 'cc-vars
  (setf (cdr (assoc 'other c-default-style)) "linux")
  (add-hook 'c-mode-common-hook 'whitespace-mode)
  (add-hook 'c-mode-common-hook 'dtrt-indent-mode)
  (add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))
  (add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'google-set-c-style))


(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'eldoc-mode)

  (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)

  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))


(with-eval-after-load 'tex-mode
  (add-hook 'latex-mode-hook 'turn-on-auto-fill)
  (add-hook 'latex-mode-hook 'turn-on-reftex)

  (setq reftex-ref-macro-prompt nil))



(setq org-enforce-todo-dependencies t)	; Need to be initialized before Org is loaded
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex) ; Dependency to AucTeX because texmathp

  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-babel-result-hide-all)

  (setq
   org-directory      "~/org/"
   org-agenda-files '("~/org/")
   org-export-backends '(ascii html latex)
   org-special-ctrl-a/e t               ; More "intuitive"
   org-tag-faces
   '(;;("@meet" . (:foreground "Chartreuse4" :weight bold :underline t))
     ("@meet" . (:foreground "mediumseagreen" :weight bold :underline t))
     ("URGENT" . (:foreground "Red" :underline t)))
   org-confirm-babel-evaluate nil       ; Org 9: #+PROPERTY: header-args :eval never-export
   org-log-done (quote time)            ; Store time when TODO -> DONE
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-src-window-setup 'current-window
   ;; org-edit-src-content-indentation 0   ; C-c ' no indent when leaves (Makefiles)
   ;; org-hide-emphasis-markers t
   org-link-search-must-match-exact-headline nil ; C-c C-l file:foo::Key1 Key2
                                                 ; then C-c C-o open and fuzzy search Key1 Key2
   org-log-into-drawer t                ; C-c C-z add note in LOGBOOK

   org-capture-templates
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


  ;; https://orgmode.org/Changes.html#org1b5e967
  (require 'org-tempo)                  ; <s TAB instead of C-c C-,
  (mapc (lambda (elem) (add-to-list 'org-structure-template-alist elem))
        (list
         '("ss" . "src")
         '("sl" . "src latex-macro")
         '("sel" . "src emacs-lisp")))


  ;; Add the support of LaTeX macro when exporting (pdf or html)
  ;; Write your LaTeX macros in source block latex-macro
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
     "\n#+HTML_HEAD_EXTRA: \\)</div>\n")))


(provide 'pkgs)

;;; pkgs.el ends here
