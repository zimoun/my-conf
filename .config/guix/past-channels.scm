(list
 (channel
  (name 'past)
  (url "https://gitlab.inria.fr/guix-hpc/guix-past.git"))
 (channel
  (name 'guix) ; avoid to recompute heavy derivations and build modules
  (url "https://git.savannah.gnu.org/git/guix.git")
  (commit "d62c9b2671be55ae0305bebfda17b595f33797f2"))) ; v1.1.0
