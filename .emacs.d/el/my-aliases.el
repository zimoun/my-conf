
;;; Be careful! ;;;
;; these are not necessary consistent with .bashrc

(defmacro defpar (var val &optional doc)
  "Quick hack to assign on the fly the variable VAR with the value VAL."
  `(if (boundp ',var)
       (setq ,var ,val)
     (defvar ,var ,val ,doc)))


(defpar my/aliases
  '(

    ;; hum? why && does not work?
    ;; instead ;
    ;; but does not neither. Hum?!
    ;; delete -G and --color=always
    ("ll" . "ls -Ahrtl")
    ("cd" . "cd $1 ; ls -Art1")
    ("mkcd" . "mkdir -p $1 ; cd $1")

    ("em" . "for i in ${eshell-flatten-list $*} {find-file $i}")
    ("ew" . "find-file-other-window $1")
    ("dir" . "my/dired $1")

    ("locate" . "locate -d ~/.cache/locate.db $1")
    ("ff" . "locate $* | grep ${pwd}")
    ("ff-here" . "find ${pwd} -type f -name $1 -print")
    ("ff-there" . "find $1 -type f -name $2 -print")
    ("ff-usr/bin/find" . "\"*find\" $1 -type f -name $2 -print")

    ("fgrep" . "find . -type f -exec grep -nH $1 {} \\;")

    ("git-grep" . "my/git-grep $1")
    ("git-grep--all" . "my/git-grep--all $1")

    ("psg" . "ps -fe | grep $1")

    ("pdfconvert2gray" . "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
           -sColorConversionStrategy=Gray \
           -dProcessColorModel=/DeviceGray -dCompatibilityLevel=1.4 \
	       -sOutputFile=${file-name-sans-extension $1}_gray.pdf \
	       $1")

    ("bibview" . "eshell/pdfview $1 ~/bib/")

    ("printer" . "lp -o media=A4 -o sides=two-sided-long-edge -o number-up=2")
    ("printer-rectoverso" . "lp -o media=A4 -o sides=two-sided-long-edge")

    ("ls-nas" . "ls -1 /run/user/1000/gvfs")
    ("cd-nas" . "cd /run/user/1000/gvfs/smb-share:server=pfiuh.nas.pfiuh.iuh.univ-paris-diderot.fr,share=$1")

    ("ag" . "ag --color-line-number '1;31' --color-path '1;35' $1")

    ;; because of Guix
    ("mupdf" . "mupdf-x11 $1")

    ("qq" . "eshell/close")
    )
  "List of cons cells containing all the aliases. See `my/aliases-define-file.'

*Warning*: `defvar' is defined only once, if the variable is not already bounded.
Otherwise, the value must be assigned with `setq'.
The macro `defpar' simply does the job.")
