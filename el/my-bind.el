
(provide 'my-bind)

(global-set-key (kbd "C-c C-r") 'my/compile-or-recompile)
(global-set-key (kbd "C-c C-t") 'my/compile-run)
(global-set-key (kbd "C-c C-e") 'my/compile-clean)

(global-set-key [?\C-$] 'ispell-region)
;;;;;;;; C-$ does not work in terminal
(global-set-key [f1] 'ispell-buffer)
(global-set-key [f11] 'flyspell-mode)
(global-set-key [f12] 'flyspell-buffer)

;; to easily erase a complete line
(global-set-key [?\C-k] 'kill-whole-line)
(global-set-key [?\C-\M-d] 'my/kill-line-or-delete-indent)

;; fix C-a when indent
(global-set-key [remap move-beginning-of-line]
                  'my/move-beginning-of-line)

;;;; plus C-x o which is working everywhere
;;;;  ie within and without X
;;;; (should free this key because conflicts with Org `org-force-cycle-archived')
(global-set-key (kbd "C-<tab>") 'other-window)

;; completion with shift-tab
(global-set-key (quote [S-tab]) (quote dabbrev-expand))


;;;;
;;;; Already binded by M-;
;;;; (see section Comments in Manual)
;;;; C-h i m Emacs m Comment Command
;;;; C-h i g (emacs)Comment Commands
;;;;
;; to comment a region (work in all the modes)
(global-set-key [?\C-c ?c] '(lambda (beg end &optional arg)
                              (interactive "*r\nP")
                              (progn
                                (message "Switch to M-; see  C-h i g (emacs)Comment Commands.")
                                (comment-region beg end arg))))
;; more C-c C-c in C-mode
(global-set-key [?\C-c ?u] '(lambda (beg end &optional arg)
                              (interactive "*r\nP")
                              (progn
                                (message "Switch to M-; see  C-h i g (emacs)Comment Commands.")
                                (uncomment-region beg end arg))))
;; (global-set-key [?\C-c ?\C-ù] 'uncomment-region)

;; to complement M-! which starts one line Shell command
(global-set-key (kbd "C-!") 'my/eshell)

;; More practical than the standard version
(global-set-key (kbd "M-c") 'my/capitalize-word)
(global-set-key (kbd "M-u") 'my/upcase-word)
(global-set-key (kbd "M-l") 'my/downcase-word)

;; M-x mode-* instead of the long name
(defalias 'mode-whitespace 'whitespace-mode)
(defalias 'mode-highlight 'global-hl-line-mode)
(defalias 'mode-glasses 'glasses-mode)
(defalias 'mode-follow 'follow-mode)
;;;; M-x mode-glasses to enable/disbale other display of:
;;;;  unreadableIdentifiersLikeThis


;; M-x cc instead of the long name
(defalias 'cc 'recompile)

;; hum? is it possible to have alias per mode
;; i.e., here only with Python
(defalias 'workon 'pyvenv-workon)


;; instead long name
;;;; do not forget to turn on (recentf-mode t)
;;;; see my-env.el
(defalias 'find-recent 'recentf-open-files)
