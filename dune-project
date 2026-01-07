(lang dune 3.20)

(using menhir 3.0)

(cram enable)

(name minisync)

(generate_opam_files true)

(source
 (github hgruniaux/minisync))

(authors "Hubert Gruniaux <hubert@gruniaux.com>")

(maintainers "Hubert Gruniaux <hubert@gruniaux.com>")

(license UNLICENSE)

(package
 (name minisync)
 (synopsis "A MiniSync compiler.")
 (description "A compiler for a ML-like synchronous programming language.")
 (depends ocaml)
 (tags
  ("minisync" "compiler" "synchronous" "ml" "programming-language")))
