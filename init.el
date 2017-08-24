
;; reduce the number of pauses due to garbage collection
;; Increases to 10MiB (instead of ~0.7MiB)
(setq gc-cons-threshold (* 10 1024 1024))

;; Add my custom folder to the elisp path
(add-to-list 'load-path (expand-file-name "el" user-emacs-directory))

;; Package system
;; ;; (if (< emacs-major-version 24)
;; ;;  (who-does-that-?)
;; ;;  (you-need-to-update))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (haskell-mode geiser ess pyvenv yasnippet magit julia-mode graphviz-dot-mode pandoc-mode markdown-mode lua-mode tuareg utop python-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
