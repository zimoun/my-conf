;;; my-functions -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(defun my/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

Should globally remap `move-beginning-of-line'.

From URL `http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/'."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))



(defun my/goto-line-with-feedback ()
  "Active `display-line-numbers-mode' when calling `goto-line'.

Adapted from URL `http://whattheemacsd.com/key-bindings.el-01.html#disqus_thread/'"
  (interactive)
  (unwind-protect
      (progn
        (when (not (boundp 'display-line-numbers-mode))
          (display-line-numbers-mode 0))
        (when (not display-line-numbers-mode)
          (display-line-numbers-mode 1))
        (call-interactively 'goto-line))))



(defun my/kill-line-or-delete-indent (&optional arg)
  "If text exists from point to end then `kill-line' else `delete-indentation'.

The optional argument ARG is provided to `kill-line'.
And `delete-indentation' joins the line below to the current point.

From URL `http://emacswiki.org/emacs/AutoIndentation'."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))



(defun my/join-line (&optional arg)
  "Join the current line to the next one and correctly set the whitespace.

The optional ARG joins the current line to previous one.

See `join-line'."
  (interactive "*P")
  (beginning-of-line)
  (forward-line 1)
  (if arg (forward-line -2))
  (if (eq (preceding-char) ?\n)
      (progn
        (delete-region (point) (1- (point)))
        ;; If the second line started with the fill prefix,
        ;; delete the prefix.
        (if (and fill-prefix
                 (<= (+ (point) (length fill-prefix)) (point-max))
                 (string= fill-prefix
                          (buffer-substring (point)
                                            (+ (point) (length fill-prefix)))))
            (delete-region (point) (+ (point) (length fill-prefix))))
        (fixup-whitespace))))



(defun my/change-case-word (fun)
  "Generic function to change the case of a word.

When the `point' is somewhere in word, first get the `backward-word'
position, second get the `forward-line' position, and last apply FUN
to these both."
  (interactive)
  (let ((beg (progn
               (backward-word)
               (point)))
        (end (progn
               (forward-word)
               (point))))
    (funcall fun beg end)))

(defun my/capitalize-word ()
  "Remap of `capitalize-word'."
  (interactive)
  (my/change-case-word 'capitalize-region))

(defun my/upcase-word ()
  "Remap of `upcase-word'."
  (interactive)
  (my/change-case-word 'upcase-region))

(defun my/downcase-word ()
  "Remap of `downcase-word'."
  (interactive)
  (my/change-case-word 'downcase-region))



(defun my/align-ws (start end)
  "Align columns by whitespace.

START and END are used by `align-regexp'."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun my/align-& (start end)
  "Align columns by ampersand (&).

START and END are used by `align-regexp'."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\&" 1 1 t))



(defun my/time-stamp ()
  "Insert current date at point.

Simpler than `org-time-stamp', but should work with any mode.
See Info node `(org)Creating timestamps' for why this format is nice.
This format is manipulable when `org-mode' is launched.

If `org-mode' is not loaded, then link to `org-time-stamp' is blank."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))



;;; https://www.emacswiki.org/emacs/ParEdit
(defvar my/ilectrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun my/ilectrify-return-if-match (arg)
  "Electrify.

If the text after the cursor matches `ilectrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the cursor
to the new line.

ARG is.

See URL `https://www.emacswiki.org/emacs/ParEdit'"
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at my/ilectrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))



(defun my/Info-reload ()
  "Info reload."
  (interactive)
  (with-eval-after-load "info"
    (let ((file Info-current-file)
          (node Info-current-node)
          (point (point)))
      (Info-revert-find-node file node)
      (goto-char point))))



(defun my/theme ()
  "Theme based on `misterioso'."
  (interactive)
  (progn
    (menu-bar-mode 1)
    (load-theme 'misterioso)
    (set-face-background 'cursor "gold")
    (set-face-background 'mode-line "steel blue")
    (set-face-background 'mode-line-inactive "gray70")
    (set-face-attribute 'region nil :background "black")))

(defun my/theme-default ()
  "Default theme."
  (interactive)
  (progn
    (menu-bar-mode 1)
    (disable-theme 'misterioso)
    (set-face-background 'cursor "black")
    (set-face-background 'mode-line "grey")
    (set-face-background 'mode-line-inactive "white smoke")
    (set-face-attribute 'region nil :background "yellow")))

(defun my/theme-blue ()
  "Theme with `LightCyan3' background-color."
  (interactive)
  (progn
    (menu-bar-mode 1)
    (disable-theme 'misterioso)
    (set-face-background 'cursor "black")
    (set-background-color "LightCyan3")
    (set-face-attribute 'region nil :background "yellow")))

(defun my/theme-gold ()
    "Theme with `LightGoldenrod3' background-color."
  (interactive)
  (progn
    (menu-bar-mode 1)
    (disable-theme 'misterioso)
    (set-face-background 'cursor "black")
    (set-background-color "LightGoldenrod3")
    (set-face-background 'mode-line "DarkGoldenrod3")
    (set-face-attribute 'region nil :background "yellow")))


(defun my/size (width height)
  "Resize easily."
  (when window-system
    (set-frame-size (selected-frame) width height)))

(defun my/desktop-screen-resize ()
  "Resize height-er for Desktop screen"
  (interactive)
  (my/size 100 75))

(defun my/laptop-screen-resize ()
  "Resize height-er for Laptop screen"
  (interactive)
  (my/size 115 55))

(defun my/normal-size ()
  "Resize large easily."
  (interactive)
  (my/size 80 36))


(defun my/bookmark-bmenu-list ()
  "Work around `bookmark-bmenu-list'.
The list is displayed in a buffer named `bookmark-bmenu-buffer'."
  (bookmark-bmenu-list)
  (switch-to-buffer
   (get-buffer-create bookmark-bmenu-buffer)))


(defun my/repeat (command)
  "Extend `repeat'.  E.g., C-x o C-x z becomes C-x o o."
  (require 'repeat)
  (let ((repeat-previous-repeated-command command)
        (repeat-message-function        #'ignore)
        (last-repeatable-command        #'repeat))
    (repeat nil)))


(defmacro defun-bug->url (name url &optional docstring)
  "Macro returning yankage #bug URL.

The `interactive' function that the macro returns is then referred by NAME.

Please provide a DOCSTRING."
  (let ((fun (intern (symbol-name name)))
        (doc (concat docstring "\n\n"
                           (format "Yankable result: `%sNUMBER'." url))))
    `(defun ,fun (number)
       ,doc
        (interactive
         (list
          (progn
            (when (not (boundp 'debbugs-gnu-bug-number))
              (setq debbugs-gnu-bug-number -2))
            (read-string
             (format "Bug number (%s): " debbugs-gnu-bug-number)
             nil nil debbugs-gnu-bug-number))))
      (let ((str (format "%s%s" ,url number)))
        (kill-new str)
        (when current-prefix-arg
          (browse-url str))
        (message (format "%s killed." str))))))

(defun-bug->url my/guix-issues "http://issues.guix.gnu.org/issue/"
          "Add URL of bug NUMBER to `kill-ring'.")
(defun-bug->url my/guix-debbugs "https://debbugs.gnu.org/cgi/bugreport.cgi?bug="
          "Add (old) URL of bug NUMBER to `kill-ring'.")


(defun my/guix-data (package)
  "Add URL of PACKAGE to `kill-ring'.

Yankable result:
`https://data.guix.gnu.org/repository/1/branch/master/package/PACKAGE/output-history'.

With `universal-argument', load URL using `browse-url'."
  (interactive "sPackage: ")
  (let ((url
         (format
          "https://data.guix.gnu.org/repository/1/branch/master/package/%s/output-history" package)))
    (kill-new url)
    (when current-prefix-arg
      (browse-url url))
    (message (format "%s killed." url))))


(defvar my/timer-instance nil
  "The `timer' used by `my/timer-start' and `my/timer-stop'.")

(defvar my/timer-counter 0
  "Internal counter to restore `mode-line' after `invert-face'.")

(defvar my/timer-time-default "30 min"
  "Default time for `my/timer-instance'.

See `my/timer-start' for recognized forms.")

(defun my/timer-func ()
  "Default function to raise the end of `my/timer-instance.'"
  (invert-face #'mode-line))

(defun my/timer-start (time)
  "Start `my/timer-instance'.

The recognized forms for TIME are 'xxxx', 'x:xx', or
'xx:xx' (military time), and 'xxam', 'xxAM', 'xxpm', 'xxPM',
'xx:xxam', 'xx:xxAM', 'xx:xxpm', or 'xx:xxPM'.

The repeat is set to one second.  Default function to apply is
set by `my/timer-func'."
  (interactive
   (list
    (progn
      (when (not (boundp 'my/timer-time-default))
        (setq my/timer-timer-default "30 min"))
      (read-string
       (format "Timer start (%s): " my/timer-time-default)
       nil nil my/timer-time-default))))
  (when (timerp my/timer-instance)
    (progn
      (my/timer-stop)
      (message "Timer restarted.")))
  (setq my/timer-instance
        (run-at-time time 1 #'(lambda ()
                                (apply 'my/timer-func nil)
                                (setq my/timer-counter
                                      (1+ my/timer-counter))))))

(defun my/timer-stop ()
  "Stop `my/timer-instance'."
  (interactive)
  (unless (eql (% my/timer-counter 2) 0)
      (apply 'my/timer-func nil))
  (when (timerp my/timer-instance)
    (cancel-timer my/timer-instance))
  (setq my/timer-instance nil)
  (setq my/timer-counter 0))

(provide 'funs)


(defun my/add-gpg-key-from (from)
  "Insert the GPG key FROM the email address.

This interactive function spawns a new `shell' and insert commands inside.
Far from perfect but does the trick.

It works only if once Notmuch is loaded."
  (interactive
   (list
    (read-string
     (format "From (%s): " (notmuch-show-get-from))
     nil nil (notmuch-show-get-from))))
  (let* ((gpg "gpg")
        (keyserver "--keyserver hkp://pool.sks-keyservers.net")
        (search "--search-keys")
        (who (format "\"%s\"" from))
        (cmd (concat
              gpg " " keyserver " " search " " who))
        (local-buffer "*gpg:add-key-from*"))
    (shell local-buffer)
    (insert "builtin cd ~/")
    (comint-send-input)
    (insert cmd)
    (comint-send-input)
    (message (format "Add GPG key of %s done." from))))


(defun my/current-time ()
  "Display humam readable `current-time' other than `current-time-string'."
  (interactive)
  (message (format-time-string "%a %b %d, %H:%M" (current-time))))


(defun my/debugs-gnu (fun-debbugs-gnu &rest args)
  "List all Guix related bugs and pending patches."
  (interactive)
  (let ((debbugs-guix "*Guix Bugs*"))
    (if (get-buffer debbugs-guix)
        (switch-to-buffer debbugs-guix)
      ;; args seems empty
      (apply fun-debbugs-gnu
             `(,debbugs-gnu-default-severities
               ,debbugs-gnu-default-packages nil t)))))


(defvar my/presentation-mode nil
  "Internal for `my/toggle-presentation-mode'.")

(defvar my/mode-line-format nil
  "Store the state of `mode-line-format'.")
(defvar my/scroll-bar-mode nil
  "Store the state of `scroll-bar-mode'.")
(defvar my/menu-bar-mode nil
  "Store the state of `menu-bar-mode'.")
(defvar my/tool-bar-mode nil
  "Store the state of `tool-bar-mode'.")

(defun my/toggle-presentation-mode ()
  "Switch to presentation mode."
  (interactive)
  (if my/presentation-mode
      (progn
       (setq
        my/presentation-mode nil
        mode-line-format my/mode-line-format)
       (set-scroll-bar-mode my/scroll-bar-mode)
       (if my/menu-bar-mode
           (menu-bar-mode 1)
         (menu-bar-mode 0))
       (if my/tool-bar-mode
           (tool-bar-mode 1)
         (tool-bar-mode 0))
       (message "Quit presentation mode: restore previous sate."))
    (progn
      (my/theme-default)
      (setq
       my/presentation-mode t
       my/mode-line-format mode-line-format
       my/scroll-bar-mode scroll-bar-mode
       my/menu-bar-mode menu-bar-mode
       my/tool-bar-mode tool-bar-mode
       mode-line-format nil)
      (set-scroll-bar-mode nil)
      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (message "Presentation mode.")))
  (force-mode-line-update)
  (redraw-display))

;;; funs.el ends here
