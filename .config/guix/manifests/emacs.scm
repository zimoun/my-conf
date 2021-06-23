
(specifications->manifest
 (append '("aspell"
           "aspell-dict-en"
           "aspell-dict-fr"
           "notmuch"                    ; here otherwise broken
           "emacs"

           ;; both required by emacs-guix
           "guix"
           "guile"
           )
         (map
          (lambda (pkg)
            (string-append "emacs-" pkg))
          '(
            "diminish"
            "yasnippet"

            "smex"
            "ivy"
	    "counsel"
            "ivy-rich"
	    "ivy-hydra"

            "magit"
            "debbugs"
            "ag"

            "org"
            "org-contrib"

            "flycheck"
            "auctex"
            "auto-dictionary-mode"
            "ox-pandoc"
            "pandoc-mode"
            "htmlize"
            "org-re-reveal"
            "typo"

            "pdf-tools"

            "paredit"
            "geiser"

            "guix"

            ;; "ess"
            "julia-mode"
            ;; octave ?
            "pyvenv"

            "haskell-mode"
            "tuareg"
            ;; "utop" ?
            "rust-mode"

            ;; "snakemake-mode" ?
            "lua-mode"
            "markdown-mode"
            "google-c-style"
            "graphviz-dot-mode"
            "skewer-mode"

            "ws-butler"
            "fill-column-indicator"
            "page-break-lines"
            "info-plus"

            "keyfreq"

            ;; "command-log-mode" ?
            ))))
