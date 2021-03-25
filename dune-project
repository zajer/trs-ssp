(lang dune 2.0)
(name ssp)
(version 2.0)
(generate_opam_files true)

(source (github zajer/trs-ssp))
(license MIT)
(authors "Piotr Cybulski <pikus3@list.pl>")
(maintainers "Piotr Cybulski <pikus3@list.pl>")
(homepage "https://github.com/zajer/trs-ssp")
(bug_reports "https://github.com/zajer/trs-ssp/issues")

(package
  (name ssp)
  (synopsis "An OCaml library for generating state reachability matrices.")
  (description "")
  (tags ("behavior policy" "state space") )
  (depends
    (dune (and :build (>= 2.0.0)))
    (ocaml (>= 4.11))
  )
)
