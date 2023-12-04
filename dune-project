(lang dune 3.11)
(name etude)
(generate_opam_files true)

(package
 (name etude)
 (synopsis "An Extension to KW's Prelude")
 (description "An Extension to KW's Prelude")
 (maintainers "Matt Teichman <teichman@uchicago.edu>")
 (authors "Matt Teichman <teichman@uchicago.edu>")
 (homepage "https://github.com/bufordrat/etude")
 (bug_reports "https://github.com/bufordrat/etude")
 (depends
   prelude
   alcotest :with-test
   qcheck :with-test)
 (source (github bufordrat/etude)))

