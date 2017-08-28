;; -*- lexical-binding: t -*-

(provide 'my-fun)


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

;; not sure this is useful
(defun my/return-newline-with-indent ()
  "Function that does its name.

(note: with-indent means if necessary)
"
  (local-set-key (kbd "RET") 'newline-and-indent)
)


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


;; from http://emacswiki.org/emacs/AutoIndentation
(defun my/kill-line-or-delete-indent (&optional arg)
  "Useful `kill-line'.

`kill-line' from point to end if text exist
or delete-indentation and so join line."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg))
)


(defun my/save-buffer-as-pdf (my-file-name)
  "Function that does its name.

  Require ps2pdf
  temporary file at /tmp/tmp.ps [not-removed]."

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

;; from http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun my/eshell ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
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
    (when (file-directory-p "~/pkg")
      (insert (concat "addpath ~/pkg/bin ; printf '\n' ; ")))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun my/eshell-quit ()
  (insert "exit")
;  (eshell-send-input)
  (delete-window))


;; from http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline194
(defun my/pick-random-doc ()
  (interactive)
    "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions."
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


;; from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun my/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
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

(defun my/comment-line ()
  "Insert by calling `my/comment-insert-char' until `fill-column'.
Then `comment-region' from `back-to-indentation' to `line-end-position'."
  (interactive)
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

Since I am not using `paredit', yet."
  (interactive "P")
  (my/move-beginning-of-line nil)
  (paredit-kill argument)
  )


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
 - if two files are marked: use them."

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
        ))))


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
