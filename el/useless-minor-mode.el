
;;; from: https://bzg.fr/en/emacs-hide-mode-line.html
;;; https://bzg.fr/en/emacs-strip-tease.html/

(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer.

Similar to `olivetti-hide-mode-line'."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden-Mode-Line mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))


;; A small minor mode to use a big fringe
(defvar center-window-mode nil)

(define-minor-mode center-window-mode
  "Minor mode to use big fringe in the current buffer.

Similar to `olivetti-mode'."
  :init-value nil
  :global t
  :variable center-window-mode
  :group 'editing-basics
  (if (not center-window-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2)))
  (when (and (called-interactively-p 'interactive)
             center-window-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Center-Window mode enabled.  "
             "Use M-x center-window-mode to display fullscreen."))))


;; Use a minimal cursor
;; (setq default-cursor-type 'hbar)

;; Get rid of the indicators in the fringe
;; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;;         fringe-bitmaps)


(provide 'useless-minor-mode)
