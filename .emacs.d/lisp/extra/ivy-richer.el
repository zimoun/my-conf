;;; ivy-richer -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ivy-rich)

(defun ivy-richer-mode ()
  "Hack to enable `ivy-rich' with customizations."
  (interactive)
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7 :face dired-ignored))
            (ivy-rich-switch-buffer-major-mode (:width 12))
            (ivy-rich-switch-buffer-path (:width
                                          (lambda (x)
                                            (ivy-rich-switch-buffer-shorten-path
                                             x
                                             (ivy-rich-minibuffer-width 0.3)))
                                          :face font-lock-doc-face)))
           :predicate
           (lambda (cand) (get-buffer cand)))

          ;; Unmodified
          counsel-find-file
          (:columns
           ((ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 30))
            (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:face font-lock-doc-face)))))
        ivy-rich-path-style 'abbrev)
  ;; Turn on the mode after ivy-rich-display-transformers-list
  (ivy-rich-mode 1))


(provide 'ivy-richer)

;;; ivy-richer.el ends here
