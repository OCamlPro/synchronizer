(lang dune 3.7)

(cram enable)

(generate_opam_files true)

(name synchronizer)

(license AGPL-3.0-or-later)

(authors "Arthur Carcano <arthur.carcano@ocamlpro.com>" "redianthus")

(maintainers "redianthus")

(source
 (github ocamlpro/synchronizer))

(package
 (name synchronizer)
 (synopsis "Synchronizer")
 (description "A synchronizer to make datastructures thread-safe.")
 (tags
  (ocaml multi-core mutex thread-safe parallel))
 (depends
  ;; build
  (ocaml
   (>= 5.2))
  ;; system packages
  ;; runtime
  (prelude
   (>= 0.5))
  ;; doc
  (odoc
   (and
    (>= 3.0.0)
    :with-doc))
  ;; test
  ;; dev
  (ocamlformat :with-dev-setup)))
