open Mille_bornes.Arena

let () =
  Random.self_init ();
  Format.open_vbox 0;
  let result = arena () in
  Format.printf "Result : @ %a" pp_endplay result;
  Format.close_box ();
  Format.printf "@."
