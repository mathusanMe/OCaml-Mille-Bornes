(lang dune 3.11)

(name mille_bornes)

(generate_opam_files true)

(source
 (uri git+https://gaufre.informatique.univ-paris-diderot.fr/selvakum/2023-ppo4-sad))

(authors "Gabin Dudillieu" "Mathusan Selvakumar" "Thomas Arrous")

(maintainers "Gabin Dudillieu" "Mathusan Selvakumar" "Thomas Arrous")

(license LICENSE)

(package
 (name mille_bornes)
 (synopsis "French board game")
 (description "An interesting french board game where you can play with your friends, it is understandable quite easily and nice to play.")
 (depends ocaml dune (odoc :with-doc) ppx_deriving (alcotest :with-test) js_of_ocaml js_of_ocaml-lwt (qcheck :with-test))
 (tags
  ("card game" "french" "mille bornes" "ocaml project")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
