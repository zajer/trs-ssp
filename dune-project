(lang dune 2.0)
(name policy)
(version 1.0)
(generate_opam_files true)

(source (github zajer/state_space_policy))
(license MIT)
(authors "Piotr Cybulski <pikus3@list.pl>")
(maintainers "Piotr Cybulski <pikus3@list.pl>")
(homepage "https://github.com/zajer/state_space_policy")
(bug_reports "https://github.com/zajer/state_space_policy/issues")

(package
  (name policy)
  (synopsis "An OCaml library for generating achievable state matrices.")
  (description "")
  (tags ("policy" "state space") )
  (depends
    (dune (and :build (>= 2.0.0)))
  )
)
