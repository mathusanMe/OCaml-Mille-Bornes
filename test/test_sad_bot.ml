open QCheck
open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine
open Mille_bornes.Sad_bot
open Utils_bot

let sad_bot_strategy =
  {
    name = "sad_bot";
    choose_card_to_play = sad_bot_choose_card_to_play;
    want_to_peek_discard_pile = sad_bot_want_to_peek_discard_pile;
    want_to_play_coup_fourre = sad_bot_want_to_play_coup_fourre;
  }

let player_gen =
  Gen.oneof
    [
      Gen.map (fun p -> Computer (p, sad_bot_strategy)) player_struct_gen;
      Gen.map (fun p -> Human p) player_struct_gen;
    ]

let test_sad_bot_choose_card_to_play =
  Test.make ~count:200
    ~name:"sad_bot_choose_card_to_play always returns Some (int, int option)"
    (make
       (Gen.triple player_gen public_informations_gen
          public_informations_list_gen))
    (fun (player, p_info, p_info_list) ->
      try
        let _ = sad_bot_choose_card_to_play player p_info p_info_list in
        true
      with
      | Speed_limit_on_the_drive_pile | Hazard_not_speed_limit_on_the_speed_pile
      | Empty_deck | Card_not_found | Hazard_on_the_drive_pile_missing ->
          true
      | _ -> false)

let test_sad_bot_want_to_draw_discard_pile =
  Test.make ~count:200
    ~name:"sad_bot_want_to_draw_discard_pile always returns Some bool"
    (make
       (Gen.quad player_gen card_gen public_informations_gen
          public_informations_list_gen))
    (fun (player, card, p_info, p_info_list) ->
      try
        let _ =
          sad_bot_want_to_peek_discard_pile player card p_info p_info_list
        in
        true
      with
      | Card_not_found -> true
      | _ -> false)

let _ =
  Random.self_init ();
  QCheck_runner.run_tests ~verbose:true
    [ test_sad_bot_choose_card_to_play; test_sad_bot_want_to_draw_discard_pile ]
