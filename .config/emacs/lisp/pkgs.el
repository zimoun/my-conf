;;; the-packages-config -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(with-eval-after-load 'dired
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
  (add-hook 'erc-mode-hook 'typo-mode)
  (setq
   erc-nick "zimoun"
   erc-default-server "irc.libera.chat"
   erc-autojoin-channels-alist '(("libera.chat"
                                  "#swh-devel"
                                  "#guix-hpc" "#guix"))
   erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
   erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE")
   erc-track-priority-faces-only 'all
   erc-track-faces-priority-list '(erc-current-nick-face erc-keyword-face))
  (define-key erc-mode-map (kbd "C-c C-s") 'erc-cmd-Q))


(with-eval-after-load 'sendmail
  (setq send-mail-function 'smtpmail-send-it))


(with-eval-after-load 'smtpmail
  (setq smtpmail-stream-type 'starttls
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-queue-mail t
        smtpmail-queue-dir "~/mail/queue/"))


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
  (add-hook 'debbugs-gnu-mode-hook #'hl-line-mode)

  (define-key debbugs-gnu-mode-map "N" 'debbugs-gnu-narrow-to-status)
  (define-key debbugs-gnu-mode-map "s" 'debbugs-gnu-search)
  (define-key debbugs-gnu-mode-map "#" 'debbugs-gnu-bugs)

  (require 'gnus-sum)
  (require 'gnus-art)
  (define-key gnus-summary-mode-map "R" 'gnus-summary-wide-reply-with-original)
  (define-key gnus-article-mode-map "R" 'gnus-summary-wide-reply-with-original)

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


(with-eval-after-load 'bookmark
  (define-key bookmark-bmenu-mode-map "L" 'bookmark-bmenu-load)
  (define-key bookmark-bmenu-mode-map "l" 'bookmark-bmenu-list)
  (define-key bookmark-bmenu-mode-map "S" 'bookmark-bmenu-save)
  (define-key bookmark-bmenu-mode-map "s" 'bookmark-bmenu-search)
  (define-key bookmark-bmenu-mode-map (kbd "TAB") #'(lambda ()
                                                      (interactive)
                                                      (forward-line)
                                                      (when (= (point) (point-max))
                                                        (goto-char (point-min)))))
  ;; C-x r b everywhere
  (define-key bookmark-bmenu-mode-map "b" 'bookmark-jump))


(with-eval-after-load 'cc-vars
  (setf (cdr (assoc 'other c-default-style)) "linux")
  (add-hook 'c-mode-common-hook 'whitespace-mode)
  ;(add-hook 'c-mode-common-hook 'dtrt-indent-mode)
  (add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))
  ;(add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'google-set-c-style))


(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'eldoc-mode)

  (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)

  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))


(with-eval-after-load 'tex-mode
  ;; latex-mode: core Emacs
  ;; LaTeX-mode: AucTeX
  (dolist (hook (list 'latex-mode-hook 'LaTeX-mode-hook))
    (add-hook hook 'turn-on-auto-fill)
    (add-hook hook 'turn-on-reftex))

  (setq font-latex-fontify-script nil   ; because of AucTeX
        reftex-ref-macro-prompt nil))



(setq org-enforce-todo-dependencies t)	; Need to be initialized before Org is loaded
(with-eval-after-load 'org
  (require 'ol-notmuch)                 ;add notmuch: as source C-c C-l

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)

  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-babel-result-hide-all)

  (add-to-list 'org-file-apps '("\\.pdf" . emacs))

  (defun my/org-templates-file (filename)
    ;; Locations of templates; path used below (diary)
    `(file ,(concat "~/.config/emacs/org-tmpl/" filename)))

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

   org-capture-templates
   (backquote
    (("t" "Todo")
     ("tt" "TODO Guix" entry
      (file+headline "~/org/todo.org" "Guix")
      ,(my/org-templates-file "todo.org"))
     ("tr" "Release" entry
      (file+headline "~/org/todo.org" "Release")
      ,(my/org-templates-file "todo.org"))
     ("th" "Hunt" entry
      (file+headline "~/org/todo.org" "Bug Hunt")
      ,(my/org-templates-file "todo-bug.org"))
     ("tg" "Guix" entry
      (file+headline "~/org/todo.org" "Guix")
      ,(my/org-templates-file "todo-simple.org"))
     ("ts" "Simple" entry
      (file+headline "~/org/todo.org" "Inbox")
      ,(my/org-templates-file "todo-simple.org") :prepend t)
     ("tw" "Work" entry
      (file+headline "~/org/work.org" "Inbox")
      ,(my/org-templates-file "todo-simple.org"))

     ("d" "Done" entry
      (file+datetree "~/org/extra-log.org")
      "* DONE %?\nCLOSED: %U")
     ("m" "Email" entry
      (file+datetree "~/org/extra-log.org")
      "* Emails  :emails:" :clock-in t :clock-keep t)

     ("M" "Meeting")
     ("Mw" "Work" entry
      (file+headline "~/org/work.org" "Inbox")
      "* TODO %?   :@meet:\n SCHEDULED: %^t")
     ("Mt" "Misc" entry
      (file+headline "~/org/todo.org" "Inbox")
      "* TODO %?   :@meet:\n SCHEDULED: %^t")

     ("o" "Other")
     ("ob" "Bookmark" entry
      (file+headline "~/org/future.org" "Bookmarks")
      ,(my/org-templates-file "bookmark.org") :prepend t)
     ("oi" "Ideas" entry
      (file+headline "~/org/future.org" "Ideas")
      ,(my/org-templates-file "idea.org") :prepend t)))

  org-capture-templates-contexts
  '(("tt" ((in-mode . "notmuch-show-mode")))
    ("tr" ((in-mode . "notmuch-show-mode")))
    ("tt" ((in-mode . "notmuch-show-mode")))
    ("th" ((in-mode . "gnus-summary-mode")))
    ("th" ((in-mode . "gnus-article-mode"))))

  org-refile-targets '((nil . (:maxlevel . 1)))

  org-agenda-custom-commands
  '(("r" "Review"
     ((agenda "" ((org-agenda-span 7)))
      (tags-todo "URGENT"
                 ((org-agenda-overriding-header "Urgent")))
      (tags-todo "@meet"
                 ((org-agenda-max-entries 2)
                  (org-agenda-sorting-strategy '(deadline-up scheduled-up))
                  (org-agenda-overriding-header "Meeting")))
      (todo "" ; -URGENT-@meet+TODO=\"TODO\"
            ((org-agenda-overriding-header "Tasks")))
      (tags "inbox"
            ((org-agenda-sorting-strategy '(deadline-up scheduled-up))
             (org-agenda-overriding-header "Unclassified"))))
     ((org-deadline-warning-days 7)
      (org-agenda-overriding-header "Review")))
    ("n" "All TODOs"
     ((agenda "")
      (alltodo "")))))

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
