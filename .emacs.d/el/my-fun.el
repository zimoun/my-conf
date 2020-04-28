;; -*- lexical-binding: t -*-



(defun my/byte-compile-el-init ()
 "Byte-compile all lisp in `user-emacs-directory' (~/.emacs.d/).

  See local file update.sh to full compilation
    Doing: package-refresh-contents then re-compile if needed.

then check:
   time emacs -l .emacs.d/init.el -batch --eval '(message \"hi\")'
   time emacs -l .emacs.d/init.elc -batch --eval '(message \"hi\")'
"
  (interactive)
  (byte-recompile-directory user-emacs-directory 0)
)

;; avoid surprises
(defun my/save-file-and-remove-filec ()
  "Add hook to remove FILENAMEC when saving."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t)
)

;; not sure this is useful. Remove ?
(defun my/return-newline-with-indent ()
  "Function that does its name.

(note: with-indent means if necessary)
"
  (local-set-key (kbd "RET") 'newline-and-indent)
)



;;;;
;; Compilation -- needs improvments
;;;; because it is a mess with the directory
(defun my/compile-or-recompile ()
  (interactive)
  (if (get-buffer "*compilation*")
      (call-interactively 'recompile)
    (progn
      (setq compile-command "make -k -C ~/")
      (call-interactively 'compile)))
)

(defun my/compile-clean ()
  (interactive)
  (setq compile-command-save compile-command)
  (setq compile-command (concat compile-command " clean "))
  (call-interactively 'recompile)
  (setq compile-command compile-command-save)
)

(defun my/compile-run ()
  (interactive)
  (if (get-buffer "*compilation*")
      (kill-buffer "*compilation*"))
  (call-interactively 'compile)
)
;;;;


(defun my/compile-a () ;(my-path)
  (interactive)
  (setq compile-command "make -k -C ")
  (setq compile-command-default "")
  (setq my-compile-command (concat compile-command compile-command-default))
  (message "%s." compile-command)
  (message "%s." my-compile-command)
  (message "%s" (equal compile-command my-compile-command))
  (if (not (equal compile-command my-compile-command))
      (recompile)
    (compilation-read-command))

)

(defun my/compile-b () ;(my-path)
  (interactive)
  (message "a-cmd: %s" compile-command)
  (compilation-read-command)
  (message "e-cmd: %s" compile-command)
)

;; from http://emacswiki.org/emacs/ModeCompile
(defun my/compilation-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code)
)

;;
;; end compilation unworking stuff
;;



(defun my/kill-line-or-delete-indent (&optional arg)
  "Useful `kill-line'.

`kill-line' from point to end if text exist
or delete-indentation and so join line.

Should be globally binded to `C-M d'.

From URL `http://emacswiki.org/emacs/AutoIndentation'.
"
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg))
)


(defun my/save-buffer-as-pdf (my-file-name)
  "Function that does its name.

  Require ps2pdf
  temporary file at /tmp/tmp.ps [not-removed].
"
  (interactive "FSave as pdf: ")

  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")

  (let ((cmd (concat "ps2pdf /tmp/tmp.ps " my-file-name)))
    (shell-command cmd))

    (kill-buffer "tmp.ps")
  (message (format "Saved: %s " my-file-name))
)


(defun my/align-ws (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t)
)

(defun my/align-& (start end)
  "Align columns by ampersand (&)."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\&" 1 1 t)
)


(defun my/eshell ()
  "Open up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier.

From URL `http://www.howardism.org/Technical/Emacs/eshell-fun.html'.
"
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         ;; (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
;;    (split-window-vertically (- height))
;;    (split-window-vertically)
;;    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    ;; trick when do not use standard Linux, and add some NetBSD packages
    ;;;; useful when running under MacOS env
    (when (file-directory-p "~/pkg")
      (insert (concat "addpath ~/pkg/bin ; printf '\n' ; ")))

    (insert (concat "ls"))
    (eshell-send-input))
)

(defun my/eshell-starter ()
  "Hack to launch EShell in Emacs client with AwesomeWM.

Start a new `eshell' session
then load the elisp file `my/aliases-define-file' containing the `my/aliases' list of cons cells.
The function `my/eshell-alias' writes down this list to the file `eshell-aliases-file'.
Last the aliases are updated by internal function."
  (interactive)
  (eshell "new")
  (load-file my/aliases-define-file)
  (my/eshell-alias my/aliases)
  (eshell-read-aliases-list)
  ;; this is not efficient at all
  ;; because it loads the theme each time it starts
  (my/setup))


;; not used. Remove ?
(defun my/eshell-quit ()
  (insert "exit")
;  (eshell-send-input)
  (delete-window)
)

(defun my/shell ()
  "Open new shell with new buffer name."
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))

(defun my/join-line (&optional arg)
  "Join this line to next and fix up whitespace at join.
With argument, join this line to previous line.

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

(defun my/list-exported-files (&optional ext dir)
  "List the files matching the extension EXT and with another one.
By default the extension EXT is \"org\" and the directory DIR is the current one."
  (let* ((there (or dir "."))
         (extension (or ext "org"))
         (regexp (concat "\." extension "$"))
         (files (directory-files there))
         (exts (seq-filter (lambda (x) (string-match regexp x)) files))
         (names (mapcar (lambda (x) (file-name-sans-extension x)) exts))
         (exts+exported (seq-filter
                         (lambda (x) (member (file-name-sans-extension x) names))
                         files)))
    (seq-filter (lambda (x) (not (string-match regexp x))) exts+exported)
    ))

;; almost never used. :-(
(defun my/pick-random-doc ()
    "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions.

From URL `http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline194'.
"
    (interactive)
    (let (result)
      (mapatoms
       (lambda (s)
         (when (and (commandp s)
                    (documentation s t)
                    (null (get s 'byte-obsolete-info))
                )
           (setq result (cons s result))
      )))
      (describe-function (elt result (random (length result)))))
)


(defun my/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

Should globally remap `move-beginning-of-line'.

From URL `http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/'
"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg)))
  )

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))
  )
)


;; Remove ?
(defun my/not-implemented-find-replace ()
  "
Not yet implemented this process to do it in once command.

Warning: `*-u` will save ALL the unsaved buffer.
therefore, it is prone error... Be careful!

M-x find-name-dired
t : dired-toogle-marks
Q : dired-do-query-replace-regexp
M-x ibuffer
*-u : ibuffer-mark-unsaved-buffers
S : ibuffer-do-save
"
  (interactive)
  (message "Do nothing for now. Info in `docstring`.")
)


(defun my/comment-insert-char (n)
  "Recursively insert `-' N times. Ends by `>8'.

Used by `my/comment-line'.
"
  (if (> n 3)
      (progn
        (insert "-")
        (my/comment-insert-char (+ n -1)))
    (insert ">8")
    )
  )

(defun my/comment-separator ()
  "Quick line separator with page-break, e.g.,
^L
;; ----------------------------------------------------------------------------->8

Insert page break to easily navigate in the file.
Insert by calling `my/comment-insert-char' until `fill-column'.
Then `comment-region' from `back-to-indentation' to `line-end-position'.

See `C-x ]' or `C-x [' for navigation with page-break.
(`forward-page' and `backward-page')
"
  (interactive)
  (insert page-delimiter)
  ;; because page-delimiter is "^^L"
  (beginning-of-line)
  (delete-char 1)
  (end-of-line)
  ;; end of ugly part
  (newline)
  (let (
        ;; (bcol (progn
        ;;         (beginning-of-line)
        ;;         (back-to-indentation)
        ;;         (current-column)))
        (beg (line-beginning-position))
        (end)
        (ecol (progn
                (end-of-line)
                (current-column))))
    (my/comment-insert-char (- fill-column ecol))
    (setq end (line-end-position))
    (comment-region beg end)
    (end-of-line)
    (newline)
  )
)


(defun my/paredit-kill (&optional argument)
  "Unused!

Since I am not using `paredit', yet.
"
  (interactive "P")
  (my/move-beginning-of-line nil)
  (paredit-kill argument)
)


(defun my/change-case-word (fun)
  "Generic function to change the case of a word.

When the `point' is somewhere in word, first get the `backward-word'
position, second get the `forward-line' position, and last apply FUN
to these both.
"
  (interactive)
  (let ((beg (progn
               (backward-word)
               (point)))
        (end (progn
               (forward-word)
               (point))))
        (funcall fun beg end)
        ))

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


;; not sure if useful ?
(defun my/dired-sort ()
  (interactive)
  (dired-sort-other "-artl"))


(defun my/dired-ediff-or-diff (universal)
  "Improve Info node `(emacs) Dired' mode by adding `ediff-files' to
the keybinding `=' through UNIVERSAL argument.

`=' binds `dired-diff' at point.

`C-u =' binds `ediff-files':
 - if no file is marked: use file at point and ask other file;
 - if one file is marked: use other file at point;
 - if two files are marked: use them.
"
  ;; "P" to provide `current-prefix-arg' as UNIVERSAL argument
  (interactive "P")

  (let ((files (dired-get-marked-files))
        file1 file2)

    (if (> (length files) 2)
        (error (format
                "Error: %d marked files, instead of 2. %s"
                (length files) (mapcar 'file-name-nondirectory files)))

      (if (eq universal nil)
          (progn
            (setq file1 (dired-get-filename t))
            (setq file2
                  (read-file-name
                   (format "[Diff] File B to compare (%s) : "
                           (file-name-nondirectory file1))
                   ;; (dired-dwim-target-directory))))
                   (dired-current-directory)))
            (dired-diff file2))

        (progn
          (when (= (length files) 1)
            (progn
              (setq file1 (car files))

              (if (string= file1 (expand-file-name (dired-get-filename t)))
                  (setq file2
                        (read-file-name
                         (format "[Ediff] File B to compare (%s) : "
                                 (file-name-nondirectory file1))
                             ;; (dired-dwim-target-directory))))
                         (dired-current-directory)))
                (setq file2 (dired-get-filename t)))
              ))

          (when (= (length files) 2)
            (progn
              (setq file1 (car files))
              (setq file2 (cadr files))))

          (ediff-files file1 file2))
        )))
)


(defun my/cut-here (beg end)
  "Box the selected region, by scissor and cut here.

For example,

--8<---------------cut here---------------start------------->8---
Here, we are.
Bye.
--8<---------------cut here---------------stop-------------->8---

Note: BEG and END are provided by (`interactive' \"r\")."

  (interactive "r")
  (let ((string "--8<---------------cut here---------------%s------------->8---"))
    (goto-char beg)
    (insert (format string "start"))
    ;;(electric-newline-and-maybe-indent)
    (newline)
    (goto-char (+ end
                  (1- (length string))
                  (length "start")))
    (insert (format string "stop-"))
    (newline)
    ;;(electric-newline-and-maybe-indent)
    ))



(defun my/time-stamp ()
  "Insert current date at point.

Simpler than `org-time-stamp', but should work with any mode.
See Info node `(org)Creating timestamps' for why this format is nice.
This format is manipulable when M-x `org-mode' is launched.

If `org-mode' is not loaded, then link to `org-time-stamp' is blank.
"
   (interactive)
   (insert (format-time-string "<%Y-%m-%d %a>"))
)

(defun my/current-time ()
  "Insert current time at point.
"
   (interactive)
   (insert (format-time-string "%H:%M"))
)



;; Ugly hack! Not sure it is the right way...
;;;; but seems do the job.
(defun my/magit-initially-hide-untracked (section)
  "Used by `magit-section-set-visibility-hook'.

See Info node `(magit)Section Visibility'.

From URL `https://emacs.stackexchange.com/questions/20754/change-the-default-visibility-of-a-magit-section'. "
  (and (not magit-insert-section--oldroot)
       (eq (magit-section-type section) 'untracked)
       'hide)
)


(defun my/org-agenda (&optional ARG ORG-KEYS RESTRICTION)
  "Ugly hack to remember how to enable the log mode with agenda view.

Because not found a way to do it automatically."
  (interactive "P")
  (org-agenda ARG ORG-KEYS RESTRICTION)
  (message "Display all entries: v l (org-agenda-log-mode)")
)


(defun my/comment-dwim (ARG)
  "Wrapper around `comment-dwim'. Because `comment-kill' is not so much useful.

If a prefix ARG is specified, then call `comment-line' (C-x C-;).
By default (C-u M-;) comment the current line.
C-u n M-; comments the n lines, starting at the current line.
Negative integer comments topward.


Note: M-2 M-; is equivalent to C-u 2 M-; see `universal-argument'.
"
  (interactive "*P")
  (if ARG
      (if (integerp ARG)
          (comment-line ARG)
        (comment-line 1))
    (comment-dwim ARG))
)

;;; https://www.emacswiki.org/emacs/ParEdit
(defvar my/ilectrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun my/ilectrify-return-if-match (arg)
  "If the text after the cursor matches `ilectrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line.

See URL `https://www.emacswiki.org/emacs/ParEdit'"
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at my/ilectrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))


(defun my/git-grep (regexp)
  "Mimick the command-line git-grep."
  (interactive "sGit grep (regexp): ")
  (grep
   (format
    "git --no-pager grep -nH --color=auto -i \"%s\" -- $(git rev-parse --show-toplevel)"
    regexp)))

(defun my/git-grep--all (regexp)
  "Mimick the command-line git-grep."
  (interactive "sGit grep (regexp): ")
  (grep
   (format
    "git rev-list --all | xargs git --no-pager grep -nH --color=auto -i \"%s\" "
    regexp)))

(defun my/find-grep (regexp &optional dir)
  "Mimick the command-line find-grep."
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
  (interactive)
  (with-eval-after-load "info"
    (let ((file Info-current-file)
          (node Info-current-node)
          (point (point)))
      (Info-revert-find-node file node)
      (goto-char point))))

(defun my/setup ()
  (interactive)
  (progn
    (menu-bar-mode 1)
    (load-theme 'misterioso)
    (set-face-background 'cursor "gold")
    (set-face-background 'mode-line "steel blue")
    (set-face-background 'mode-line-inactive "gray70")
    (set-face-attribute 'region nil :background "black")))

(defun my/setup-blue ()
  (interactive)
  (progn
    (menu-bar-mode 1)
    (disable-theme 'misterioso)
    (set-background-color "LightCyan3")))

(defun my/setup-gold ()
  (interactive)
  (progn
    (menu-bar-mode 1)
    (disable-theme 'misterioso)
    (set-background-color "LightGoldenrod3")
    (set-face-attribute 'region nil :background "yellow")))

(defun my/buffer-file-name ()
  (interactive)
  (message (buffer-file-name)))

(defun my/apply-patch ()
  (interactive)
  (let ((gnus-summary-pipe-output-default-command
         "cd ~/work/guix/guix/ && git am"))
    (gnus-summary-pipe-output)))

(defun my/guix-issue (number)
  (interactive "sBug number: ")
  (let ((str (format "http://issues.guix.gnu.org/issue/%s" number)))
    (kill-new str)
    (message (format "%s yanked." str))))

(defun my/guix-debbug (number)
  (interactive
   (list
    (read-string
     (format "Bug number (%s): " debbugs-gnu-bug-number)
     nil nil debbugs-gnu-bug-number)))
  (let ((str (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s" number)))
    (kill-new str)
    (message (format "%s yanked." str))))


;; adapted from:
;;; http://whattheemacsd.com/key-bindings.el-01.html#disqus_thread
 (defun my/goto-line-with-feedback ()
   "Active `display-line-numbers-mode' when calling `goto-line'."
   (interactive)
   (unwind-protect
       (progn
         (when (not (boundp 'display-line-numbers-mode))
           (display-line-numbers-mode 0))
         (when (not display-line-numbers-mode)
           (display-line-numbers-mode 1))
         (call-interactively 'goto-line))))

(defun my/double-space-sentence (arg)
  "Add space to the end of a sentence."
  (interactive "p")
  (let ((sentence-end-double-space nil))
    (loop for i from 1 to arg do
          (forward-sentence 1)
          (insert " "))))

;; From: http://stackoverflow.com/a/18814469/519736
(defun my/copy-buffer-file-name (choice)
  "Copy the buffer-file-name to the kill-ring"
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

(provide 'my-fun)