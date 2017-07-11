
(provide 'my-still-in-dev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORK in PROGRESS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; when improved, then include in my-pkg
(use-package gmsh-getdp
  :defer t
  :mode ("\\.geo\\'" . gmsh-mode)
  :mode ("\\.pro\\'" . getdp-mode)
  :config
  (message "Still in dev. Gmsh/GetDP mode")
  (message "todo: IMPROVE Gmsh/GetDP mode!! Issue with several buffer")
 ;; incompatible with layout-restore
;; (defun pro-mode-common-hook ()
;;   (define-key c++-mode-base-map "\C-c ?m" 'pro-complete-symbol)
;;   )
;; (add-hook 'pro-mode-hook
;; 	  '(lambda()
;; 	    (local-set-key [backtab] 'pro-complete-symbol)
;; 	    ))
;; (global-set-key [backtab] 'pro-complete-symbol)
  )

(autoload 'notmuch "notmuch" "notmuch mail" t)
(setq message-kill-buffer-on-exit t)

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
(setq mail-specify-envelope-from t)
(setq message-sendmail-f-is-evil nil)
(setq mail-envelope-from 'header)
(setq message-sendmail-envelope-from 'header)

(add-hook 'notmuch-show-hook
          (lambda ()
            (font-lock-add-keywords nil
               '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
                  (0 'message-multiply-quoted-text-face))
                 ("^[ \t]*>[ \t]*>.*$"
                  (0 'message-double-quoted-text-face))))))

(set-register ?o (cons 'file "~/my.org"))
(set-register ?e (cons 'file "~/.emacs.d"))
(put 'narrow-to-region 'disabled nil)

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds
        '(
          "https://lwn.net/headlines/rss"
          ;"http://nullprogram.com/feed/"
          ;"http://planet.emacsen.org/atom.xml"
          )
        )
  )

;; (use-package erc
;;   :ensure t
;;   :init
;;   (require 'tls)
;;   (erc-tls
;;    :server "irc.debian.org"
;;    :port 6697
;;    :nick "zimoun"
;;    :full-name "simon")
;; )
