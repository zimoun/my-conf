
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
;;;;  ie within  and without X
(global-set-key (kbd "C-<tab>") 'other-window)

;; completion with shift-tab
(global-set-key (quote [S-tab]) (quote dabbrev-expand))

;; to comment a region (work in all the modes)
(global-set-key [?\C-c ?c] 'comment-region)
;; more C-c C-c in C-mode
(global-set-key [?\C-c ?u] 'uncomment-region)
;; (global-set-key [?\C-c ?\C-ù] 'uncomment-region)

;; to complement M-! which starts one line Shell command
(global-set-key (kbd "C-!") 'my/eshell)

;; More practical than the standard version
(global-set-key (kbd "M-c") 'my/capitalize-word)
(global-set-key (kbd "M-u") 'my/upcase-word)
(global-set-key (kbd "M-l") 'my/downcase-word)

;; M-x wms instead of the long name
(defalias 'wsm 'whitespace-mode)

;; M-x gm to enable/disbale other display of unreadableIdentifiersLikeThis
(defalias 'gm 'glasses-mode)

;; M-x cc instead of the long name
(defalias 'cc 'recompile)

;; hum? is it possible to have alias per mode
;; i.e., here only with Python
(defalias 'workon 'pyvenv-workon)

;; because it seems more matural to me
(defalias 'run-elisp 'ielm)
