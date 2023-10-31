(lang dune 3.7)
(name etude)
(generate_opam_files true)

(package
 (name etude)
 (synopsis "Experimental additions to KW's Prelude")
 (description "Experimental additions to KW's Prelude")
 (maintainers "Matt Teichman <teichman@uchicago.edu>")
 (authors "Matt Teichman <teichman@uchicago.edu>")
 (homepage "https://github.com/bufordrat/etude")
 (bug_reports "https://github.com/bufordrat/etude")
 (depends prelude)
 (source (github bufordrat/etude)))

