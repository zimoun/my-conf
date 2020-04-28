
(specifications->manifest
 (append '("aspell"
           "aspell-dict-en"
           "aspell-dict-fr"
           "emacs")
         (map
          (lambda (pkg)
            (string-append "emacs-" pkg))
          '(
            "diminish"
            "yasnippet"

            "smex"
            "ivy"
            "ivy-rich"

            "magit"
            "debbugs"
            "ag"

            "org"
            "org-contrib"

            "flycheck"
            "auctex"
            "ox-pandoc"
            "pandoc-mode"
            "htmlize"
            "org-re-reveal"

            "pdf-tools"

            "paredit"
            "geiser"

            "guix"

            "ess"
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

            "elfeed"

            ;; "command-log-mode" ?
            ))))
