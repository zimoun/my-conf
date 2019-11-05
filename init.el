
;; reduce the number of pauses due to garbage collection
;; Increases to 10MiB (instead of ~0.7MiB)
(setq gc-cons-threshold (* 10 1024 1024))

;; Add my custom folder to the elisp path
(add-to-list 'load-path (expand-file-name "el" user-emacs-directory))

;; Avoid issue loading old bytecode instead of newer source
(setq load-prefer-newer t)

;; Package system
;; ;; (if (< emacs-major-version 24)
;; ;;  (who-does-that-?)
;; ;;  (you-need-to-update))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             ;; e.g., `use-package' is not in ELPA
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     ;; Add org-plus-contrib
	     '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; ;; Do not forget to refresh the contents, time to time.
;; ;; Otherwise, the locally listed package should be not installable,
;; ;; since the remotely available package should be updated.
;; (package-refresh-contents)

;; Boostrap `use-package' by John Wiegley
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; use it
(require 'use-package)
;; aggregate time spent in each package
;;; try M-x use-package-report
(setq use-package-compute-statistics t)
;; verbose the loading
(setq use-package-verbose t)

;; Fix issue from included Org version vs Latest one
;;; see http://lists.gnu.org/archive/html/emacs-orgmode/2019-03/msg00094.html
;; (unless (package-installed-p 'org-plus-contrib)
;;   (package-refresh-contents)
;; (package-install 'org-plus-contrib))
(use-package org
  :ensure org-plus-contrib		; ensure the last version of Org
  :defer t)


;; more or less useful functions
(use-package my-fun
  :init
  (message "(shell) See local file update.sh")
  (message "      or  M-x my/byte-compile-el-init")
)

;; general settings (menu, scroll-bar etc.)
(use-package my-env)

;; useful packages
(use-package my-pkg)

;; nicer bindings
(use-package my-bind)

;; Work in progress
(use-package my-still-in-dev
  :defer t
  :config
  (message "See my-still-in-dev.el.")
)

;; useless minor mode
(use-package useless-minor-mode)

;; Move automatic custom to special file
;; ;; avoid to pollute this file
;; ;; and custom.el is not versionned
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load-file custom-file))
