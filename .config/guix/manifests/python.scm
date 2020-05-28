
(specifications->manifest
 (append
  '("python"
    )
  (map
   (lambda (pkg)
     (string-append "python-" pkg))
   '("ipython"
     "numpy"
     "matplotlib"
     "scipy"
     "biopython"
  ))))
