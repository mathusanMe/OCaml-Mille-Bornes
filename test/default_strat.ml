open Mille_bornes.Teams_engine

let strat =
  {
    name = "strat";
    choose_card_to_play = (fun _ _ _ -> Some (0, None));
    want_to_peek_discard_pile = (fun _ _ _ _ -> Some false);
    want_to_play_coup_fourre = (fun _ _ _ _ -> Some true);
  }
