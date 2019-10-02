;; -*- lexical-binding: t -*-


;; UTF8 everywhere
; (setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; mainly to avoid python-mode issues
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

;; to simplify the question-answer process
(defalias 'yes-or-no-p 'y-or-n-p)

;; delete the file *~ when quiting
; (setq make-backup-files nil)

;; backup management ?
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
)

;; to remove the initial starting message
(setq inhibit-startup-message t)

;; delete the initial message in *scratch*
(setq initial-scratch-message nil)

;; turn on ELisp instead of Lisp to *scratch*
(setq initial-major-mode 'emacs-lisp-mode)

;; visual bell
(setq visible-bell t)

;;; status bar info : mode-line
(setq display-time-24hr-format 1
      display-time-mail-string ""
      display-time-load-average nil
      display-time-default-load-average nil)
(display-time-mode 1)
;; note: before set and then activate, otherwise weird !?

;; to give the name of the open file to the window
(setq frame-title-format '(buffer-file-name "%f"))
;;;; ou "Emacs: %b (%f)"

;; to see the life in colours
(global-font-lock-mode 1)

;; to underline the selected area
(setq transient-mark-mode t)

;; load theme
(load-theme 'misterioso)

;; because the default is not enough visible
(add-to-list 'default-frame-alist '(cursor-color . "gold"))

;; increase the contrast
(set-face-background 'mode-line "steel blue")
(set-face-background 'mode-line-inactive "gray70")

;; to change highlight of the selection
(set-face-attribute 'region nil :background "black")

;; to change the background, sometimes eyes are really tired
;(set-background-color "LightGoldenrod3")
;;(set-background-color "LightCyan3")

;; even the shell deserves colours
;; ;; instead of ASCII(-art?)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; to remove the icons bar
(tool-bar-mode 0)

;; what?!? who use the latteral bar to scroll ?
(scroll-bar-mode 0)

;; remove the menu
(if (display-graphic-p)
    (menu-bar-mode 1)
    (menu-bar-mode 0))


;; the cursor does not blink anymore
(blink-cursor-mode -1)
;; comment: with terminal, Emacs is not managing
;; the cursor but the terminal is doing itself.
;; Therefore, the parameters of the terminal
;; need to be modified to fix the blink.
;; With X, Emacs manages the cursor in this Lisp works.

;; to change the split of ediff / nice with tiling
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; tabs with 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 200 4))


;; compilation stuff
(setq compilation-scroll-output 'first-error)
(setq compilation-exit-message-function 'my/compilation-autoclose)

;; to move with Shift-arrow
(windmove-default-keybindings)
   ;;;; do not work with all the terminals

;; automatic completion
(abbrev-mode t)

;; recent file
;; Really slow down when closing Emacs
;; So commented for now
;(recentf-mode t)

;; the new buffer are by default in Text mode
;(setq major-mode 'text-mode)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
   ;;;; the last line is an improvment with auto-fill

;; if browse some web ??
(setq w3m-use-cookies t)

;; hi-lock-mode: avoid to prompt the color
(setq hi-lock-auto-select-face t)


;; character per line
(setq-default fill-column 72)

;; used by Calendar
;; location: Paris
(setq calendar-latitude +48.8)
(setq calendar-longitude +2.3)
(setq calendar-location-name "Paris")


;; find my compiled stuff
(setenv "PATH" (concat "~/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("~/local/bin")))
(setenv "LIBRARY_PATH"
        (concat "/home/simon/local/lib:" (getenv "LIBRARY_PATH")))
(setenv "LD_LIBRARY_PATH"
        (concat "/home/simon/local/lib:" (getenv "LD_LIBRARY_PATH")))



;; find Guix stuff
(defun my/-addpath (input-list separator output-list &optional not-rev)
  "Concatenate INPUT-LIST to OUTPUT-LIST (helper function).

The recursive concatenation reverses the order.
Instead of applying the function `reverse' when calling, an option eases the job.
If NOT-REV is set to `t', then INPUT-LIST is not reversed.
The aim is to reverse at the first call, and so, preserve the order.

SEPARATOR is a string, .e.g., \":\".

See `eshell/addpath'. Except that the order is not finely controlled.
(defun my/addpath (&rest args)
  (setenv \"PATH\" (my/-addpath args \":\" (getenv \"PATH\")))
"
  (let (lst)
    (if not-rev
        (setq lst input-list)
      (setq lst (reverse input-list)))
    (if (equal input-list '())
        output-list
      (let
          ((elem (expand-file-name (car lst)))
           (rest (cdr lst)))
        (my/-addpath
         rest separator
         (concat elem separator output-list)
         t)))))
;; $HOME is not well recognized by `expand-file-name', but ~/ is.
(setq guix-paths
      '("~/.config/guix/current/bin"
        "~/.guix-profile/bin"
        "~/.guix-profile/sbin"))
(setenv "PATH" (my/-addpath guix-paths ":" (getenv "PATH")))
(setq exec-path (append exec-path guix-paths))
(setenv "LIBRARY_PATH"
        (concat "/home/simon/local/lib:" (getenv "LIBRARY_PATH")))
(setenv "LD_LIBRARY_PATH"
        (concat "/home/simon/local/lib:" (getenv "LD_LIBRARY_PATH")))

;; Guix variable
(setq safe-local-variable-values
      '((bug-reference-bug-regexp
         .
         "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")))


;; useful to start fullscreen
;;; for example with exec emacs in .xinitrc
;(set-frame-parameter nil 'fullscreen 'fullboth)

(provide 'my-env)
