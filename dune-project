(lang dune 3.16)
  (generate_opam_files true)
  (authors "Tobi Ijiyemi" "Vrinda Sehgal" "Timmy Ventura")
  (maintainers "oijiyem2@jh.edu" "vsehgal2@jh.edu" "tventur4@jh.edu")
  (package
   (name poker) ; put in your own project name here
   (allow_empty)
   (synopsis "An OCaml library for playing Poker")
   (description "An interface for playing Texas Hold 'Em poker in terminal, powered by an OCaml library")
   (depends
    (ounit2 (>= 2.2.7)) ; for each opam dependency list the version
    (core (>= 0.17.1)) ; `opam list core` will display which version of core you have
 ))