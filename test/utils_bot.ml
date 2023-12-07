open QCheck
open Default_strat
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine

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
    (fun name hand ->
      let player_init =
        List.nth
          (get_players_from
             (List.nth
                (init_teams [ (name, strat); ("Useless", strat) ] false)
                0))
          0
      in
      set_hand_from player_init hand)
    Gen.string_printable deck_of_card_gen

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

let team_init =
  List.nth (init_teams [ ("Useless1", strat); ("Useless2", strat) ] false) 0

let public_informations_gen =
  Gen.map
    (fun ( speed_limit_pile,
           drive_pile,
           distance_cards,
           safety_area,
           coup_fouree_cards ) ->
      let new_team_init =
        List.fold_left (fun t c -> use_card t c) team_init speed_limit_pile
      in
      let new_team_init =
        List.fold_left (fun t c -> use_card t c) new_team_init drive_pile
      in
      let new_team_init =
        List.fold_left (fun t c -> use_card t c) new_team_init distance_cards
      in
      let new_team_init =
        List.fold_left (fun t c -> use_card t c) new_team_init safety_area
      in
      let final_new_team_init =
        List.fold_left
          (fun t c ->
            use_coup_fouree t
              (match c with Safety c -> c | _ -> EmergencyVehicle))
          new_team_init coup_fouree_cards
      in
      get_public_informations_from final_new_team_init)
    Gen.(
      tup5 speed_pile_gen drive_pile_gen distance_card_deck_gen safety_area_gen
        safety_area_gen)

let public_informations_list_gen = Gen.list public_informations_gen
