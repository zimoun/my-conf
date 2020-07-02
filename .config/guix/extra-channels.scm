(cons*
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix.git")
  (branch "master"))
 (channel               ; .guix.channel depends on channel:
  (name 'bimsb-nonfree) ; github.com/BIMSBbioinfo/guix-bimsb.git
  (url "https://github.com/BIMSBbioinfo/guix-bimsb-nonfree.git")
  (branch "master"))
 ;; (channel
 ;;  (name 'bimsb)
 ;;  (url "https://github.com/BIMSBbioinfo/guix-bimsb.git")
 ;;  (branch "master"))
 (channel
  (name 'past)
  (url "https://gitlab.inria.fr/guix-hpc/guix-past.git")
  (branch "master"))
 %default-channels)
