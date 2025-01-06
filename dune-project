(lang dune 3.17)

(name lisp)

(generate_opam_files true)

(source
 (github MustafaAamir/lisexp))

(authors "Mustafa Aamir <mustafaaamir.github.io>")


(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lisp)
 (synopsis "A lisp interpreter")
 (description "")
 (depends ocaml base dune ocamlformat stdio)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
