open Teams_engine

type endplay = Win of team | GiveUpInGame of team | GiveUpInit

let pp_endplay _ _ = (* TODO *) ()
let player_teletype_choose_card_to_play_card _ _ _ = (* TODO *) (0, None)
let player_teletype_want_to_peek_discard_pile _ _ _ _ = (* TODO *) true
let player_teletype_want_to_play_coup_fourre _ _ _ _ = (* TODO *) true
let initial_bot_choose_card_to_play _ _ _ = (* TODO *) (0, None)
let initial_bot_want_to_peek_discard_pile _ _ _ _ = (* TODO *) true
let initial_bot_want_to_play_coup_fourre _ _ _ _ = (* TODO *) true

let initial_strategy =
  {
    name = "Initial strategy";
    choose_card_to_play = initial_bot_choose_card_to_play;
    want_to_peek_discard_pile = initial_bot_want_to_peek_discard_pile;
    want_to_play_coup_fourre = initial_bot_want_to_play_coup_fourre;
  }

let pp_strategy_list _ _ = ()

let init_teams (* TODO *) () =
  [ init_team_with_one_computer "name" initial_strategy 0 ]

let init_board (* TODO *) () = None
let play_move_player b = (* TODO *) Some b
let arena () = (* TODO *) ()
