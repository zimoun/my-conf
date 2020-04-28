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



(defun my/save-buffer-as-pdf (my-file-name)
  "My first ELisp function: save the buffer as a pdf file named MY-FILE-NAME.

  Require that ps2pdf is in the path (not checked).
  The temporary file at /tmp/tmp.ps is not removed."
  (interactive "FSave as pdf: ")

  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")

  (let ((cmd (concat "ps2pdf /tmp/tmp.ps " my-file-name)))
    (shell-command cmd))

  (kill-buffer "tmp.ps")
  (message (format "Saved: %s " my-file-name)))



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



(defun my/list-exported-files (&optional ext dir)
  "List the files matching the extension EXT and with another one.

By default the EXT is \"org\" and the directory DIR is the current one."
  (let* ((there (or dir "."))
         (extension (or ext "org"))
         (regexp (concat "\." extension "$"))
         (files (directory-files there))
         (exts (seq-filter (lambda (x) (string-match regexp x)) files))
         (names (mapcar (lambda (x) (file-name-sans-extension x)) exts))
         (exts+exported (seq-filter
                         (lambda (x) (member (file-name-sans-extension x) names))
                         files)))
    (seq-filter (lambda (x) (not (string-match regexp x))) exts+exported)))



(defun my/time-stamp ()
  "Insert current date at point.

Simpler than `org-time-stamp', but should work with any mode.
See Info node `(org)Creating timestamps' for why this format is nice.
This format is manipulable when `org-mode' is launched.

If `org-mode' is not loaded, then link to `org-time-stamp' is blank."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>"))
  )

(defun my/current-time ()
  "Insert current time at point."
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun my/org-agenda (&optional ARG ORG-KEYS RESTRICTION)
  "Ugly hack to remember how to enable the log mode with agenda view.

ARG ORG-KEYS RESTRICTION.

Because not found a way to do it automatically."
  (interactive "P")
  (org-agenda ARG ORG-KEYS RESTRICTION)
  (message "Display all entries: v l (org-agenda-log-mode)"))


(defun my/comment-dwim (ARG)
  "Wrapper around `comment-dwim'.  Because `comment-kill' is not so much useful.

If a prefix ARG is specified, then call `comment-line' (C-x C-;
By default (C-u M-;) comment the current line.
C-u n M-; comments the n lines, starting at the current line.
Negative integer comments topward.

Note: M-2 M-; is equivalent to C-u 2 M-; see `universal-argument'."
  (interactive "*P")
  (if ARG
      (if (integerp ARG)
          (comment-line ARG)
        (comment-line 1))
    (comment-dwim ARG)))

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


(defun my/git-grep (regexp)
  "Mimick the command line `git-grep'.  REGEXP."
  (interactive "sGit grep (regexp): ")
  (grep
   (format
    "git --no-pager grep -nH --color=auto -i \"%s\" -- $(git rev-parse --show-toplevel)"
    regexp)))

(defun my/git-grep--all (regexp)
  "Mimick the command line `git-grep'.  REGEXP."
  (interactive "sGit grep (regexp): ")
  (grep
   (format
    "git rev-list --all | xargs git --no-pager grep -nH --color=auto -i \"%s\" "
    regexp)))

(defun my/find-grep (regexp &optional dir)
  "Mimick the command line `find-grep'.  REGEXP and DIR."
  (interactive "sFind grep (regexp): ")
  (let ((there (if dir
                   dir
                 default-directory)))
    (if (file-directory-p there)
        (progn
          (grep
           (format
            "find %s -type f -exec grep --color -nH --null -e \"%s\" \{\} +"
            there regexp))
          (switch-to-buffer "*grep*"))
      (message "Error: %s is not a directory." there))))


(defun my/Info-reload ()
  "Info reload."
  (interactive)
  (with-eval-after-load "info"
    (let ((file Info-current-file)
          (node Info-current-node)
          (point (point)))
      (Info-revert-find-node file node)
      (goto-char point))))



(defun my/setup ()
  "Arf."
  (interactive)
  (progn
    (menu-bar-mode 1)
    (load-theme 'misterioso)
    (set-face-background 'cursor "gold")
    (set-face-background 'mode-line "steel blue")
    (set-face-background 'mode-line-inactive "gray70")
    (set-face-attribute 'region nil :background "black")))

(defun my/setup-blue ()
  "Arf."
  (interactive)
  (progn
    (menu-bar-mode 1)
    (disable-theme 'misterioso)
    (set-background-color "LightCyan3")))

(defun my/setup-gold ()
  "Arf."
  (interactive)
  (progn
    (menu-bar-mode 1)
    (disable-theme 'misterioso)
    (set-background-color "LightGoldenrod3")
    (set-face-attribute 'region nil :background "yellow")))



(defun my/apply-patch ()
  "C-| then cd <path/to/guix> && git am."
  (interactive)
  (let ((gnus-summary-pipe-output-default-command
         "cd ~/src/guix/guix/ && git am"))
    (gnus-summary-pipe-output)))

(defun my/guix-issue (number)
  "Yank to `kill-ring' the url of the bug NUMBER."
  (interactive "sBug number: ")
  (let ((str (format "http://issues.guix.gnu.org/issue/%s" number)))
    (kill-new str)
    (message (format "%s yanked." str))))

(defun my/guix-debbug (number)
  "Yank to `kill-ring' the url of the bug NUMBER."
  (interactive
   (list
    (progn
      (when (not (boundp 'debbugs-gnu-bug-number))
        (setq debbugs-gnu-bug-number -2))
      (read-string
       (format "Bug number (%s): " debbugs-gnu-bug-number)
       nil nil debbugs-gnu-bug-number))))
  (let ((str (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s" number)))
    (kill-new str)
    (message (format "%s yanked." str))))



(defun my/buffer-file-name ()
  "Display the variable `buffer-file-name'."
  (interactive)
  (message (buffer-file-name)))



(defun my/copy-buffer-file-name (choice)
  "Copy the variable `buffer-file-name' to the `kill-ring'.

The argument CHOICE selects the full path (f), directory (d) or name (n).

From URL `http://stackoverflow.com/a/18814469/519736'"
  (interactive "cCopy buffer-name (f) Full, (d) Directory, (n) Name? ")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s yanked." new-kill-string)
      (kill-new new-kill-string))))


(provide 'funs)

;;; funs.el ends here
