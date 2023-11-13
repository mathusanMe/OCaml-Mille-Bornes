open QCheck
open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine
open Mille_bornes.Sad_bot

let sad_bot_strategy =
  {
    name = "sad_bot";
    choose_card_to_play = sad_bot_choose_card_to_play;
    want_to_peek_discard_pile = sad_bot_want_to_draw_discard_pile;
    want_to_play_coup_fourre = sad_bot_want_to_play_coup_fourre;
  }

let hazard_card_drive_pile_gen =
  Gen.oneofl [ Stop; Accident; OutOfGas; FlatTire ]

let hazard_card_speed_pile_gen = Gen.oneofl [ SpeedLimit ]

let hazard_card_gen =
  Gen.oneofl [ Stop; Accident; OutOfGas; FlatTire; SpeedLimit ]

let safety_card_gen =
  Gen.oneofl [ EmergencyVehicle; FuelTruck; PunctureProof; DrivingAce ]

let remedy_card_gen = Gen.oneofl [ Repairs; Gas; SpareTire; EndOfSpeedLimit ]
let distance_card_gen = Gen.oneofl [ D25; D50; D75; D100; D200 ]

let card_gen =
  Gen.oneof
    [
      Gen.map (fun h -> Hazard h) hazard_card_gen;
      Gen.map (fun s -> Safety s) safety_card_gen;
      Gen.map (fun r -> Remedy r) remedy_card_gen;
      Gen.map (fun d -> Distance d) distance_card_gen;
    ]

let rec hazard_card_to_card list =
  match list with
  | [] -> []
  | h :: t ->
      if List.mem h [ Stop; Accident; OutOfGas; FlatTire; SpeedLimit ] then
        Hazard h :: hazard_card_to_card t
      else hazard_card_to_card t

let rec safety_card_to_card list =
  match list with
  | [] -> []
  | h :: t ->
      if List.mem h [ EmergencyVehicle; FuelTruck; PunctureProof; DrivingAce ]
      then Safety h :: safety_card_to_card t
      else safety_card_to_card t

let rec distance_card_to_card list =
  match list with
  | [] -> []
  | h :: t ->
      if List.mem h [ D25; D50; D75; D100; D200 ] then
        Distance h :: distance_card_to_card t
      else distance_card_to_card t

let deck_of_card_gen = Gen.list card_gen

let player_struct_gen =
  Gen.map2
    (fun name hand -> { name; hand })
    Gen.string_printable deck_of_card_gen

let player_gen =
  Gen.oneof
    [
      Gen.map (fun p -> Computer (p, sad_bot_strategy)) player_struct_gen;
      Gen.map (fun p -> Human p) player_struct_gen;
    ]

let drive_pile_gen =
  Gen.map hazard_card_to_card
    (Gen.oneof
       [
         Gen.map (fun c -> [ c ]) hazard_card_drive_pile_gen;
         Gen.map (fun _ -> []) Gen.unit;
       ])

let speed_pile_gen =
  Gen.map hazard_card_to_card
    (Gen.oneof
       [
         Gen.map (fun c -> [ c ]) hazard_card_speed_pile_gen;
         Gen.map (fun _ -> []) Gen.unit;
       ])

let safety_area_gen =
  Gen.map safety_card_to_card
    (Gen.list_size
       (Gen.oneof
          [
            Gen.map (fun _ -> 0) Gen.unit;
            Gen.map (fun _ -> 1) Gen.unit;
            Gen.map (fun _ -> 2) Gen.unit;
            Gen.map (fun _ -> 3) Gen.unit;
            Gen.map (fun _ -> 4) Gen.unit;
          ])
       safety_card_gen)

let distance_card_deck_gen =
  Gen.map distance_card_to_card (Gen.list distance_card_gen)

let public_informations_gen =
  Gen.map
    (fun ( id,
           speed_limit_pile,
           drive_pile,
           distance_cards,
           safety_area,
           coup_fouree_cards,
           score ) ->
      {
        id;
        speed_limit_pile;
        drive_pile;
        distance_cards;
        safety_area;
        coup_fouree_cards;
        score;
      })
    Gen.(
      tup7 int speed_pile_gen drive_pile_gen distance_card_deck_gen
        safety_area_gen safety_area_gen int)

let public_informations_list_gen = Gen.list public_informations_gen

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
          sad_bot_want_to_draw_discard_pile player card p_info p_info_list
        in
        true
      with
      | Card_not_found -> true
      | _ -> false)

let _ =
  QCheck_runner.run_tests ~verbose:true
    [ test_sad_bot_choose_card_to_play; test_sad_bot_want_to_draw_discard_pile ]
