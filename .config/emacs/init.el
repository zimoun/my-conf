;;; my-config --- endless tweaks
;;; Commentary:
;;; https://github.com/zimoun/my-conf

;;; Code:


(setq
 gc-cons-threshold (* 10 1024 1024)     ; Reduce garbage collection
 load-prefer-newer t                    ; Load .el instead of .elc

 lisp-path   (expand-file-name "lisp" user-emacs-directory)
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 kill-buffer-query-functions nil        ; Simplify question-answer process

 backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
 backup-by-copying   t    ; Don't delink hardlinks
 version-control     t    ; Use version numbers on backups
 delete-old-versions t    ; Automatically delete excess backups
 kept-new-versions   5
 kept-old-versions   5

 initial-scratch-message nil
 initial-major-mode 'fundamental-mode    ; Mode of *scratch*
 visible-bell t

 transient-mark-mode t                  ; Highlight region

 column-number-indicator-zero-based nil

 calendar-week-start-day 1

 require-final-newline t
 compilation-scroll-output 'first-error

 hippie-expand-try-functions-list '(try-expand-dabbrev
                                    try-expand-all-abbrevs
                                    try-expand-dabbrev-all-buffers
                                    try-expand-dabbrev-from-kill
                                    try-complete-file-name-partially
                                    try-complete-file-name
                                    try-expand-list
                                    try-expand-line
                                    try-complete-lisp-symbol-partially
                                    try-complete-lisp-symbol)

 copyright-names-regexp
 (format "%s <%s>" "Simon Tournier" "zimon.toutoune@gmail.com")

 user-full-name "zimoun"
 user-mail-address "zimon.toutoune@gmail.com"

 gnus-directory "/tmp/News/"
 message-auto-save-directory "/tmp/News/"
 message-kill-buffer-on-exit t
 message-cite-reply-position 'above
 message-citation-line-function 'message-insert-formatted-citation-line
 message-citation-line-format "On %a, %d %b %Y at %R, %f wrote:"
 ;; message-signature "" ;"GPG key = XXXX XXXX XXXX XXXX XXXX XXXX XXXX XXXX XXXX XXXX"

 vc-follow-symlinks t
 find-file-visit-truename t

 revert-without-query '(".+pdf$")

 keyfreq-file      (expand-file-name "keyfreq.keyfreq" user-emacs-directory)
 keyfreq-file-lock (expand-file-name "keyfreq.lock"    user-emacs-directory)

 frame-title-format '(buffer-file-name "%f"))

(setq-default
 indent-tabs-mode nil
 fill-column 78)

(defalias 'yes-or-no-p         'y-or-n-p)
(defalias 'list-buffers        'ibuffer)
(defalias 'list-directory      'dired)

(defalias 'mode-follow         'follow-mode)
(defalias 'mode-fci            'fci-mode)

(defalias 'run-elisp 'ielm)
(defalias 'cc 'recompile)

(add-to-list 'load-path lisp-path)
(let ((default-directory lisp-path))
  (normal-top-level-add-subdirs-to-load-path))
(load custom-file t)           ; Report no error if `custom-file' does not exist

(column-number-mode t)
(abbrev-mode t)                ; Completion
(global-font-lock-mode t)      ; See the life in colours
(blink-cursor-mode -1)         ; Emacs -nw is not managing the cursor,
                                        ; but the terminal is doing itself.
                                        ; Therefore, the terminal parameters
                                        ; need to be modified to fix the blink.
(load-theme 'misterioso)
(add-to-list 'default-frame-alist '(cursor-color . "gold")) ; Color of cursor
(set-face-attribute 'region nil :background "black")        ; Color of region
(set-face-background 'mode-line "steel blue")               ; More blue
(set-face-background 'mode-line-inactive "gray70")          ; More gray
(add-to-list 'default-frame-alist       ; Change the default font
             '(font . "DejaVu Sans Mono-11"))
                                        ; Should require fontconf & font-dejavu
                                        ; then: fc-cache -rv eventually.
(show-paren-mode)
(electric-pair-mode)
(electric-indent-mode)
(savehist-mode)

(global-set-key (kbd "C-h C-f") 'find-function) ; Rebind (view-emacs-FAQ)
(global-set-key (kbd "M-o")     'other-window)  ; Warning with
                                                ;`ibuffer-visit-buffer-1-window'
(global-set-key (kbd "C-k")     'kill-whole-line)
(global-set-key (kbd "C-x c s") 'isearch-forward)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-$")     'ispell-region)


(require 'reffubi)			; Set ibuffer
(require 'funs)

(advice-add 'split-window-right :after #'balance-windows) ; Resize after C-x 3
(global-set-key [remap move-beginning-of-line]
                'my/move-beginning-of-line) ; Fix C-a when indent
(global-set-key [remap goto-line]
                'my/goto-line-with-feedback)
(global-set-key (kbd "C-M-d") 'my/kill-line-or-delete-indent)
(global-set-key (kbd "M-^") 'my/join-line)
(global-set-key (kbd "M-c") 'my/capitalize-word)
(global-set-key (kbd "M-u") 'my/upcase-word)
(global-set-key (kbd "M-l") 'my/downcase-word)
(global-set-key (kbd "C-!") (lambda ()
                              (interactive)
                              (shell (generate-new-buffer-name "*shell*"))))

(global-set-key (kbd "C-x o") (lambda ()
                              (interactive)
                              (my/repeat 'other-window)))
(global-set-key (kbd "C-x [") (lambda ()
                              (interactive)
                              (my/repeat 'backward-page)))
(global-set-key (kbd "C-x ]") (lambda ()
                              (interactive)
                              (my/repeat 'forward-page)))


(require 'pkgs)

(global-set-key (kbd "C-c a")
                (lambda (&optional ARG ORG-KEYS RESTRICTION)
                  (interactive)
                  (org-agenda ARG ORG-KEYS RESTRICTION)
                  (message
                   "Display all entries: v l (org-agenda-log-mode)")))
(global-set-key (kbd "C-c t") 'org-capture)

(global-set-key (kbd "C-c l") 'bookmark-bmenu-list)


(require 'more-pkgs)

(yas-global-mode)
(pdf-tools-install)
(ivy-mode)
(ws-butler-global-mode)
(global-page-break-lines-mode)          ; Convert ^L (C-q l) to pretty lines
(keyfreq-mode)                          ; Track frequencies for bottleneck
(keyfreq-autosave-mode)

(global-set-key (kbd "C-s")      'swiper-isearch)
(global-set-key (kbd "M-x")      'counsel-M-x)
(global-set-key (kbd "C-x C-f")  'counsel-find-file)
(global-set-key (kbd "C-x b")    'ivy-switch-buffer) ; `counsel-switch-buffer'
                                                     ;  unlikely adds preview
(global-set-key (kbd "M-y")      'counsel-yank-pop)
(global-set-key (kbd "C-h f")    'counsel-describe-function)
(global-set-key (kbd "C-h v")    'counsel-describe-variable)
(global-set-key (kbd "C-c s")    'counsel-ag)
(global-set-key (kbd "C-c d")    'ag-dired)

(global-set-key (kbd "C-c g")    'magit-status) ; `magit-file-mode-map': (C-x g)
(global-set-key (kbd "C-c f")    'counsel-git)

(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

(global-set-key (kbd "C-c m") 'notmuch)
(global-set-key (kbd "C-x m") (lambda ()
                                (interactive)
                                (require 'notmuch)
                                (notmuch-mua-new-mail)))

(setenv "PAGER" "cat")
