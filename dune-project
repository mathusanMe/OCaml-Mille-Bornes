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
 (synopsis "An interesting french board game")
 (description "An interesting french board game like le bon vin you know the french traditional drink")
 (depends ocaml dune)
 (tags
  ("addictive" "french" "game" "best")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
