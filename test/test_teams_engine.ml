open Mille_bornes.Teams_engine
open Mille_bornes.Cards_engine
open Utils_teams_engine
open Utils_cards_engine

let test_have_same_contents_team =
  Alcotest.test_case "test have_same_contents_team with different teams" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" false
        (have_same_contents_team team_with_one_player team_with_two_players))

let test_replace_player_in =
  Alcotest.test_case "test replace_player_in and same_team" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let p1 = get_current_player_from team_with_one_player in
         let team_1 = team_with_one_player in
         have_same_contents_team team_1 (replace_player_in team_1 p1)))

let test_get_names =
  Alcotest.test_case "test get_names on team1 and team6" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_names_from team1 = [ "name1"; "name2" ]
        && get_names_from team6 = [ "Thomas"; "Mathusan" ]))

let test_set_next_player_and_get_current_player1 =
  Alcotest.test_case
    "test set_next_player and get_current_player on team with one computer \
     player"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_player
           (get_current_player_from team_with_one_player)
           (get_current_player_from (set_next_player_from team_with_one_player))))

let test_set_next_player_and_get_current_player2 =
  Alcotest.test_case
    "test set_next_player and get_current_player on team with one computer \
     player and one human player"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (*Checks if the first player to play and the third player to play is the same.
          Also check that the second player to play and the fourth player to play is the same.
          Also check that the first player to play and the second players to play are not the same.
          Also check that the second to play players and the third to play players are not the same.*)
        (equal_player
           (get_current_player_from team_with_two_players)
           (get_current_player_from
              (set_next_player_from
                 (set_next_player_from team_with_two_players)))
        && equal_player
             (get_current_player_from
                (set_next_player_from team_with_two_players))
             (get_current_player_from
                (set_next_player_from
                   (set_next_player_from
                      (set_next_player_from team_with_two_players))))
        && (not
              (equal_player
                 (get_current_player_from team_with_two_players)
                 (get_current_player_from
                    (set_next_player_from team_with_two_players))))
        && not
             (equal_player
                (get_current_player_from
                   (set_next_player_from team_with_two_players))
                (get_current_player_from
                   (set_next_player_from
                      (set_next_player_from team_with_two_players))))))

let test_set_next_player1 =
  Alcotest.test_case
    "test if the index of the current player remains at 0 in a team with one \
     player"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_current_player_id_from team_with_one_player = 0
        && get_current_player_id_from
             (set_next_player_from team_with_one_player)
           = 0))

let test_set_next_player2 =
  Alcotest.test_case
    "test if the index of the current player changes correctly in a team with \
     2 players"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_current_player_id_from team_with_two_players = 0
        && get_current_player_id_from
             (set_next_player_from team_with_two_players)
           = 1
        && get_current_player_id_from
             (set_next_player_from (set_next_player_from team_with_two_players))
           = 0
        && get_current_player_id_from
             (set_next_player_from
                (set_next_player_from
                   (set_next_player_from team_with_two_players)))
           = 1))

let test_get_current_player_from =
  Alcotest.test_case "test get_current_player_from" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_name_from (get_current_player_from team_with_two_players) = "Gabin"
        && get_name_from
             (get_current_player_from
                (set_next_player_from team_with_two_players))
           = "Mathusan"))

let test_does_player_have_this_name_in_team_list1 =
  Alcotest.test_case
    "test does_player_have_this_name_in_team_list with empty list" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" false
        (does_player_have_this_name_in_team_list "name" []
        || does_player_have_this_name_in_team_list "" []))

let test_does_player_have_this_name_in_team_list2 =
  Alcotest.test_case
    "test does_player_have_this_name_in_team_list with not existing name on \
     team list"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" false
        (does_player_have_this_name_in_team_list "test"
           [ team1; team2; team3; team4; team5; team6 ]
        || does_player_have_this_name_in_team_list ""
             [ team1; team2; team3; team4; team5; team6 ]
        || does_player_have_this_name_in_team_list "another test"
             [ team1; team6 ]))

let test_does_player_have_this_name_in_team_list3 =
  Alcotest.test_case
    "test does_player_have_this_name_in_team_list with existing name on team \
     list"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (does_player_have_this_name_in_team_list "name1"
           [ team1; team2; team3; team4; team5; team6 ]
        && does_player_have_this_name_in_team_list "name2"
             [ team1; team2; team3; team4; team5; team6 ]
        && does_player_have_this_name_in_team_list "Thomas"
             [ team1; team2; team3; team4; team5; team6 ]
        && does_player_have_this_name_in_team_list "Mathusan"
             [ team1; team2; team3; team4; team5; team6 ]))

let test_pp_team1 =
  Alcotest.test_case "test pp_team on team_with_two_players without hand" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) :\n\
         Gabin (player with strategy strat)\n\
         Mathusan (player with strategy strat)\n\
         Score : 0\n\
         Driving Zone : \n\
         Top of speed limit pile : (empty);\n\n\
         Top of drive pile : (empty);\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : (empty);\n\
        \               \n\
         Coup fourree cards : (empty);\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team false false) team_with_two_players))

let test_pp_team2 =
  Alcotest.test_case "test pp_team on team_with_two_players with hand" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         Gabin (player with strategy strat)\n\
         hand : (empty);\n\
        \       \n\
         Mathusan (player with strategy strat)\n\
         hand : (empty);\n\
        \       \n\
         Score : 0\n\
         Driving Zone : \n\
         Top of speed limit pile : (empty);\n\n\
         Top of drive pile : (empty);\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : (empty);\n\
        \               \n\
         Coup fourree cards : (empty);\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team true false) team_with_two_players))

let test_pp_team3 =
  Alcotest.test_case "test pp_team on team_with_one_player and with hand" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         Thomas (player with strategy strat)\n\
         hand : (empty);\n\
        \       \n\
         Score : 0\n\
         Driving Zone : \n\
         Top of speed limit pile : (empty);\n\n\
         Top of drive pile : (empty);\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : (empty);\n\
        \               \n\
         Coup fourree cards : (empty);\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team true false) team_with_one_player))

let test_pp_team4 =
  Alcotest.test_case "test pp_team on team3 with hand" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         name1 (player with strategy strat)\n\
         hand : (empty);\n\
        \       \n\
         name2 (player with strategy strat)\n\
         hand : (empty);\n\
        \       \n\
         Score : 200\n\
         Driving Zone : \n\
         Top of speed limit pile : Speed limit;\n\n\
         Top of drive pile : Accident;\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : 0. Fuel truck;\n\
        \               \n\
         Coup fourree cards : 0. Emergency vehicle;\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team true false) team3))

let test_pp_team5 =
  Alcotest.test_case "test pp_team on team6 with hand" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         Thomas (player with strategy strat)\n\
         hand : 0. Repairs;\n\
        \       1. Speed limit;\n\
        \       2. Out of gas;\n\
        \       3. Driving ace;\n\
        \       4. 100;\n\
        \       5. 200;\n\
        \       \n\
         Mathusan (player with strategy strat)\n\
         hand : 0. Drive;\n\
        \       1. Gas;\n\
        \       2. 25;\n\
        \       3. 75;\n\
        \       4. 100;\n\
        \       5. 200;\n\
        \       \n\
         Score : 625\n\
         Driving Zone : \n\
         Top of speed limit pile : End of speed limit;\n\n\
         Top of drive pile : Accident;\n\n\
         Distance cards : 0. 25;\n\
        \                 1. 100;\n\
        \                 2. 100;\n\
        \                 3. 200;\n\
        \                 \n\
         Safety cards : 0. Fuel truck;\n\
        \               1. Driving ace;\n\
        \               \n\
         Coup fourree cards : 0. Emergency vehicle;\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team true false) team6))

let test_pp_team_with_hand_of1 =
  Alcotest.test_case "test pp_team_with_hand_of on team3 with hand of name2"
    `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck of name1 :\n\
         name1 (player with strategy strat)\n\
         hand : (empty);\n\
        \       \n\
         name2 (player with strategy strat)\n\
         Score : 200\n\
         Driving Zone : \n\
         Top of speed limit pile : Speed limit;\n\n\
         Top of drive pile : Accident;\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : 0. Fuel truck;\n\
        \               \n\
         Coup fourree cards : 0. Emergency vehicle;\n\
        \                     \n\n"
        (Format.asprintf "%a"
           (pp_team_with_hand_of (List.hd (get_players_from team3)))
           team3))

let test_pp_team_with_hand_of2 =
  Alcotest.test_case "test pp_team_with_hand_of on team6 with hand of Mathusan"
    `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck of Mathusan :\n\
         Thomas (player with strategy strat)\n\
         Mathusan (player with strategy strat)\n\
         hand : 0. Drive;\n\
        \       1. Gas;\n\
        \       2. 25;\n\
        \       3. 75;\n\
        \       4. 100;\n\
        \       5. 200;\n\
        \       \n\
         Score : 625\n\
         Driving Zone : \n\
         Top of speed limit pile : End of speed limit;\n\n\
         Top of drive pile : Accident;\n\n\
         Distance cards : 0. 25;\n\
        \                 1. 100;\n\
        \                 2. 100;\n\
        \                 3. 200;\n\
        \                 \n\
         Safety cards : 0. Fuel truck;\n\
        \               1. Driving ace;\n\
        \               \n\
         Coup fourree cards : 0. Emergency vehicle;\n\
        \                     \n\n"
        (Format.asprintf "%a"
           (pp_team_with_hand_of (List.hd (List.tl (get_players_from team6))))
           team6))

let test_pp_public_informations_list =
  Alcotest.test_case
    "test pp_public_informations_list on a list containing team1-6" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "0. Driving Zone : \n\
        \   Top of speed limit pile : (empty);\n\
        \   \n\
        \   Top of drive pile : (empty);\n\
        \   \n\
        \   Distance cards : (empty);\n\
        \                    \n\
        \   Safety cards : (empty);\n\
        \                  \n\
        \   Coup fourree cards : (empty);\n\
        \                        \n\
        \   \n\
         1. Driving Zone : \n\
        \   Top of speed limit pile : Speed limit;\n\
        \   \n\
        \   Top of drive pile : Drive;\n\
        \   \n\
        \   Distance cards : (empty);\n\
        \                    \n\
        \   Safety cards : 0. Fuel truck;\n\
        \                  \n\
        \   Coup fourree cards : (empty);\n\
        \                        \n\
        \   \n\
         2. Driving Zone : \n\
        \   Top of speed limit pile : Speed limit;\n\
        \   \n\
        \   Top of drive pile : Accident;\n\
        \   \n\
        \   Distance cards : (empty);\n\
        \                    \n\
        \   Safety cards : 0. Fuel truck;\n\
        \                  \n\
        \   Coup fourree cards : 0. Emergency vehicle;\n\
        \                        \n\
        \   \n\
         3. Driving Zone : \n\
        \   Top of speed limit pile : Speed limit;\n\
        \   \n\
        \   Top of drive pile : Stop;\n\
        \   \n\
        \   Distance cards : (empty);\n\
        \                    \n\
        \   Safety cards : (empty);\n\
        \                  \n\
        \   Coup fourree cards : 0. Emergency vehicle;\n\
        \                        \n\
        \   \n\
         4. Driving Zone : \n\
        \   Top of speed limit pile : End of speed limit;\n\
        \   \n\
        \   Top of drive pile : Drive;\n\
        \   \n\
        \   Distance cards : (empty);\n\
        \                    \n\
        \   Safety cards : (empty);\n\
        \                  \n\
        \   Coup fourree cards : (empty);\n\
        \                        \n\
        \   \n\
         5. Driving Zone : \n\
        \   Top of speed limit pile : End of speed limit;\n\
        \   \n\
        \   Top of drive pile : Accident;\n\
        \   \n\
        \   Distance cards : 0. 25;\n\
        \                    1. 100;\n\
        \                    2. 100;\n\
        \                    3. 200;\n\
        \                    \n\
        \   Safety cards : 0. Fuel truck;\n\
        \                  1. Driving ace;\n\
        \                  \n\
        \   Coup fourree cards : 0. Emergency vehicle;\n\
        \                        \n\
        \   \n"
        (Format.asprintf "%a" pp_public_informations_list
           (List.map
              (fun t -> get_public_informations_from t)
              [ team1; team2; team3; team4; team5; team6 ])))

let test_pp_names_of_team_list =
  Alcotest.test_case "test pp_names_of_team_list on a list with team1-6" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Team 0 : name1;name2;\n\
         Team 1 : name1;name2;\n\
         Team 2 : name1;name2;\n\
         Team 3 : name1;name2;\n\
         Team 4 : name1;name2;\n\
         Team 5 : Thomas;Mathusan;\n\n"
        (Format.asprintf "%a" pp_names_of_team_list
           [ team1; team2; team3; team4; team5; team6 ]))

let public_informations_is_clear public_informations =
  get_speed_limit_pile_from public_informations = []
  && get_drive_pile_from public_informations = []
  && get_distance_cards_from public_informations = []
  && get_safety_area_from public_informations = []
  && get_coup_fouree_cards_from public_informations = []

let test_init_team_with_one_player =
  Alcotest.test_case "initialisation of a team of 1 player " `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length (get_players_from team_with_one_player) = 1
        &&
        let player = List.nth (get_players_from team_with_one_player) 0 in
        get_hand_from player = []
        && get_name_from player = "Thomas"
        && team_with_one_player |> get_public_informations_from
           |> get_score_from = 0
        && get_current_player_id_from team_with_one_player = 0
        && public_informations_is_clear
             (get_public_informations_from team_with_one_player)))

let test_init_team_with_two_players =
  Alcotest.test_case "initialisation of a team of 2 computer players " `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length (get_players_from team_with_two_players) = 2
        &&
        let player1 = List.nth (get_players_from team_with_two_players) 0
        and player2 = List.nth (get_players_from team_with_two_players) 1 in
        get_hand_from player1 = []
        && get_hand_from player2 = []
        && get_name_from player1 = "Gabin"
        && get_name_from player2 = "Mathusan"
        && team_with_two_players |> get_public_informations_from
           |> get_score_from = 0
        && get_current_player_id_from team_with_two_players = 0
        && public_informations_is_clear
             (get_public_informations_from team_with_two_players)))

let test_has_safety_to_counter_hazard_on_his_hand1 =
  Alcotest.test_case
    "check if a human player with all safety cards can counter all hazards"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Stop; SpeedLimit; OutOfGas; FlatTire; Accident ]
        |> List.for_all (fun card ->
               has_safety_to_counter_hazard_on_his_hand human1 card)))

let test_has_safety_to_counter_hazard_on_his_hand2 =
  Alcotest.test_case
    "check if a human player who does not have a safety card cannot counter \
     hazards"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Stop; SpeedLimit; OutOfGas; FlatTire; Accident ]
        |> List.for_all (fun card ->
               not (has_safety_to_counter_hazard_on_his_hand human2 card))))

let test_has_safety_to_counter_hazard_on_his_hand3 =
  Alcotest.test_case
    "check if a computer player with all safety cards can counter all hazards"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Stop; SpeedLimit; OutOfGas; FlatTire; Accident ]
        |> List.for_all (fun card ->
               has_safety_to_counter_hazard_on_his_hand computer1 card)))

let test_has_safety_to_counter_hazard_on_his_hand4 =
  Alcotest.test_case
    "check if a computer player who does not have a safety card cannot counter \
     hazards"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Stop; SpeedLimit; OutOfGas; FlatTire; Accident ]
        |> List.for_all (fun card ->
               not (has_safety_to_counter_hazard_on_his_hand computer2 card))))

let test_is_usable_hazard_card1 =
  Alcotest.test_case
    "check if you can use all the hazards on a team with your drive_pile and \
     speed_limite_pile empty"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Hazard Stop; Hazard OutOfGas; Hazard FlatTire; Hazard Accident ]
         |> List.for_all (fun card ->
                not (is_usable_card (get_public_informations_from team1) card))
        && [ Hazard SpeedLimit ]
           |> List.for_all (fun card ->
                  is_usable_card (get_public_informations_from team1) card)))

let test_is_usable_hazard_card2 =
  Alcotest.test_case
    "check that a hazard can be used on a team with safety cards and already \
     attacked in without speed_limit_pile"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Hazard SpeedLimit; Hazard OutOfGas ]
         |> List.for_all (fun card ->
                not (is_usable_card (get_public_informations_from team2) card))
        && [ Hazard Stop; Hazard FlatTire; Hazard Accident ]
           |> List.for_all (fun card ->
                  is_usable_card (get_public_informations_from team2) card)))

let test_is_usable_hazard_card3 =
  Alcotest.test_case
    "check if a team is attacked in its drive_pile and is protected by an \
     Safety EmergencyVehicle"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Hazard Stop;
           Hazard SpeedLimit;
           Hazard OutOfGas;
           Hazard FlatTire;
           Hazard Accident;
         ]
        |> List.for_all (fun card ->
               not (is_usable_card (get_public_informations_from team3) card))))

let test_is_usable_hazard_card4 =
  Alcotest.test_case
    "Test if the EmergencyVehicle card blocks SpeedLimit and Stop cards" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Hazard Stop; Hazard SpeedLimit ]
         |> List.for_all (fun card ->
                not (is_usable_card (get_public_informations_from team4) card))
        && [ Hazard OutOfGas; Hazard FlatTire; Hazard Accident ]
           |> List.for_all (fun card ->
                  is_usable_card (get_public_informations_from team4) card)))

let test_is_usable_hazard_card5 =
  Alcotest.test_case
    "check that a hazard can be used on a team that has a remedy card above \
     drive_pile and speed_limit_pile"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Hazard Stop;
           Hazard SpeedLimit;
           Hazard OutOfGas;
           Hazard FlatTire;
           Hazard Accident;
         ]
        |> List.for_all (fun card ->
               is_usable_card (get_public_informations_from team5) card)))

let test_use_hazard_card =
  Alcotest.test_case "test use_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team1 (Hazard SpeedLimit) in
         let res1 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Hazard SpeedLimit ]
         in
         let team = use_card team (Hazard SpeedLimit) in
         let res2 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Hazard SpeedLimit; Hazard SpeedLimit ]
         in
         let team = use_card team (Hazard OutOfGas) in
         let res3 =
           get_drive_pile_from (get_public_informations_from team)
           = [ Hazard OutOfGas ]
         in
         let team = use_card team (Hazard FlatTire) in
         let res4 =
           get_drive_pile_from (get_public_informations_from team)
           = [ Hazard FlatTire; Hazard OutOfGas ]
         in
         let team = use_card team (Hazard Accident) in
         let res5 =
           get_drive_pile_from (get_public_informations_from team)
           = [ Hazard Accident; Hazard FlatTire; Hazard OutOfGas ]
         in
         let team = use_card team (Hazard Stop) in
         let res6 =
           get_drive_pile_from (get_public_informations_from team)
           = [ Hazard Stop; Hazard Accident; Hazard FlatTire; Hazard OutOfGas ]
         in
         res1 && res2 && res3 && res4 && res5 && res6))

let test_is_usable_distance_card1 =
  Alcotest.test_case
    "checks if a team cannot advance because the team can't drive" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Distance D25;
           Distance D50;
           Distance D75;
           Distance D100;
           Distance D200;
         ]
        |> List.for_all (fun card ->
               not (is_usable_card (get_public_informations_from team1) card))))

let test_is_usable_distance_card2 =
  Alcotest.test_case "checks if a team has Hazard SpeedLimit" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Distance D25; Distance D50 ]
         |> List.for_all (fun card ->
                is_usable_card (get_public_informations_from team2) card)
        && [ Distance D75; Distance D100; Distance D200 ]
           |> List.for_all (fun card ->
                  not (is_usable_card (get_public_informations_from team2) card))
        ))

let test_is_usable_distance_card3 =
  Alcotest.test_case "checks if a team has Hazard on his drive_pile" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Distance D25;
           Distance D50;
           Distance D75;
           Distance D100;
           Distance D200;
         ]
        |> List.for_all (fun card ->
               not (is_usable_card (get_public_informations_from team3) card))))

let test_is_usable_distance_card4 =
  Alcotest.test_case
    "Test if the EmergencyVehicle card blocks SpeedLimit and Stop cards" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Distance D25;
           Distance D50;
           Distance D75;
           Distance D100;
           Distance D200;
         ]
        |> List.for_all (fun card ->
               is_usable_card (get_public_informations_from team4) card)))

let test_is_usable_distance_card5 =
  Alcotest.test_case
    "Test with Remedy on top of speed_limit_pile and drive_pile" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Distance D25;
           Distance D50;
           Distance D75;
           Distance D100;
           Distance D200;
         ]
        |> List.for_all (fun card ->
               is_usable_card (get_public_informations_from team5) card)))

let test_use_distance_card =
  Alcotest.test_case "Test with use_distance_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team5 (Distance D25) in
         let res1 =
           get_distance_cards_from (get_public_informations_from team)
           = [ Distance D25 ]
           && get_score_from (get_public_informations_from team) = 25
         in
         let team = use_card team (Distance D50) in
         let res2 =
           get_distance_cards_from (get_public_informations_from team)
           = [ Distance D25; Distance D50 ]
           && get_score_from (get_public_informations_from team) = 75
         in
         let team = use_card team (Distance D75) in
         let res3 =
           get_distance_cards_from (get_public_informations_from team)
           = [ Distance D25; Distance D50; Distance D75 ]
           && get_score_from (get_public_informations_from team) = 150
         in
         let team = use_card team (Distance D100) in
         let res4 =
           get_distance_cards_from (get_public_informations_from team)
           = [ Distance D25; Distance D50; Distance D75; Distance D100 ]
           && get_score_from (get_public_informations_from team) = 250
         in
         let team = use_card team (Distance D200) in
         let res5 =
           get_distance_cards_from (get_public_informations_from team)
           = [
               Distance D25;
               Distance D50;
               Distance D75;
               Distance D100;
               Distance D200;
             ]
           && get_score_from (get_public_informations_from team) = 450
         in
         let team = use_card team (Distance D75) in
         let res6 =
           get_distance_cards_from (get_public_informations_from team)
           = [
               Distance D25;
               Distance D50;
               Distance D75;
               Distance D75;
               Distance D100;
               Distance D200;
             ]
           && get_score_from (get_public_informations_from team) = 525
         in
         res1 && res2 && res3 && res4 && res5 && res6))

let test_is_usable_safety_card1 =
  Alcotest.test_case "check if a team has not yet used safety_card" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Safety EmergencyVehicle;
           Safety FuelTruck;
           Safety PunctureProof;
           Safety DrivingAce;
         ]
        |> List.for_all (fun card ->
               is_usable_card (get_public_informations_from team1) card)))

let test_is_usable_safety_card2 =
  Alcotest.test_case
    "checks if a team has used a safety_card in safety_area and \
     coup_fouree_cards"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Safety EmergencyVehicle; Safety FuelTruck ]
         |> List.for_all (fun card ->
                not (is_usable_card (get_public_informations_from team3) card))
        && [ Safety PunctureProof; Safety DrivingAce ]
           |> List.for_all (fun card ->
                  is_usable_card (get_public_informations_from team3) card)))

let test_use_safety_card =
  Alcotest.test_case "test use_safety_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team1 (Safety FuelTruck) in
         let res1 =
           get_safety_area_from (get_public_informations_from team)
           = [ Safety FuelTruck ]
         in
         let team = use_card team (Safety EmergencyVehicle) in
         let res2 =
           get_safety_area_from (get_public_informations_from team)
           = [ Safety EmergencyVehicle; Safety FuelTruck ]
         in
         let team = use_card team (Safety DrivingAce) in
         let res3 =
           get_safety_area_from (get_public_informations_from team)
           = [ Safety EmergencyVehicle; Safety FuelTruck; Safety DrivingAce ]
         in
         res1 && res2 && res3))

let test_use_coup_fouree =
  Alcotest.test_case "test use_coup_fouree" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_coup_fouree team1 FuelTruck in
         let res1 =
           get_coup_fouree_cards_from (get_public_informations_from team)
           = [ Safety FuelTruck ]
           && get_score_from (get_public_informations_from team) = 200
         in
         let team = use_coup_fouree team EmergencyVehicle in
         let res2 =
           get_coup_fouree_cards_from (get_public_informations_from team)
           = [ Safety EmergencyVehicle; Safety FuelTruck ]
           && get_score_from (get_public_informations_from team) = 400
         in
         let team = use_coup_fouree team DrivingAce in
         let res3 =
           get_coup_fouree_cards_from (get_public_informations_from team)
           = [ Safety EmergencyVehicle; Safety FuelTruck; Safety DrivingAce ]
           && get_score_from (get_public_informations_from team) = 600
         in
         res1 && res2 && res3))

let test_is_usable_remedy_card1 =
  Alcotest.test_case
    "You can’t play any remedy if you have no hazard except drive" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Remedy Drive ]
         |> List.for_all (fun card ->
                is_usable_card (get_public_informations_from team1) card)
        && [
             Remedy EndOfSpeedLimit;
             Remedy Gas;
             Remedy SpareTire;
             Remedy Repairs;
           ]
           |> List.for_all (fun card ->
                  not (is_usable_card (get_public_informations_from team1) card))
        ))

let test_is_usable_remedy_card2 =
  Alcotest.test_case
    "Check if the safety_area blocks hazards so you cannot use a Remedy if the \
     hazard is already countered."
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Remedy EndOfSpeedLimit ]
         |> List.for_all (fun card ->
                is_usable_card (get_public_informations_from team2) card)
        && [ Remedy Drive; Remedy Gas; Remedy SpareTire; Remedy Repairs ]
           |> List.for_all (fun card ->
                  not (is_usable_card (get_public_informations_from team2) card))
        ))

let test_is_usable_remedy_card3 =
  Alcotest.test_case
    "Check if the coup_fouree_cards block hazards so you cannot use a Remedy \
     if the hazard is already countered."
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Remedy Repairs ]
         |> List.for_all (fun card ->
                is_usable_card (get_public_informations_from team3) card)
        && [
             Remedy Drive; Remedy EndOfSpeedLimit; Remedy Gas; Remedy SpareTire;
           ]
           |> List.for_all (fun card ->
                  not (is_usable_card (get_public_informations_from team3) card))
        ))

let test_is_usable_remedy_card4 =
  Alcotest.test_case
    "Test if the EmergencyVehicle card blocks SpeedLimit and Stop cards. So \
     you cannot use a Remedy if the hazard is already countered."
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Remedy Drive;
           Remedy EndOfSpeedLimit;
           Remedy Gas;
           Remedy SpareTire;
           Remedy Repairs;
         ]
        |> List.for_all (fun card ->
               not (is_usable_card (get_public_informations_from team4) card))))

let test_is_usable_remedy_card5 =
  Alcotest.test_case
    "Check if all hazards are blocked by remedy. You cannot use remedy" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([
           Remedy Drive;
           Remedy EndOfSpeedLimit;
           Remedy Gas;
           Remedy SpareTire;
           Remedy Repairs;
         ]
        |> List.for_all (fun card ->
               not (is_usable_card (get_public_informations_from team5) card))))

let test_use_remedy_card =
  Alcotest.test_case "test use_remedy_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team1 (Remedy EndOfSpeedLimit) in
         let res1 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Remedy EndOfSpeedLimit ]
         in
         let team = use_card team (Remedy EndOfSpeedLimit) in
         let res2 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
         in
         let team = use_card team (Remedy Gas) in
         let res3 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && get_drive_pile_from (get_public_informations_from team)
              = [ Remedy Gas ]
         in
         let team = use_card team (Remedy SpareTire) in
         let res4 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && get_drive_pile_from (get_public_informations_from team)
              = [ Remedy SpareTire; Remedy Gas ]
         in
         let team = use_card team (Remedy Repairs) in
         let res5 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && get_drive_pile_from (get_public_informations_from team)
              = [ Remedy Repairs; Remedy SpareTire; Remedy Gas ]
         in
         let team = use_card team (Remedy Drive) in
         let res6 =
           get_speed_limit_pile_from (get_public_informations_from team)
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && get_drive_pile_from (get_public_informations_from team)
              = [ Remedy Drive; Remedy Repairs; Remedy SpareTire; Remedy Gas ]
         in
         res1 && res2 && res3 && res4 && res5 && res6))

let test_nth_hand_player =
  Alcotest.test_case "test nth_hand_player with good input" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let player =
           set_hand_from player_test
             [
               Safety EmergencyVehicle;
               Remedy EndOfSpeedLimit;
               Hazard OutOfGas;
               Distance D200;
             ]
         in
         nth_hand_player player 0 = Safety EmergencyVehicle
         && nth_hand_player player 1 = Remedy EndOfSpeedLimit
         && nth_hand_player player 2 = Hazard OutOfGas
         && nth_hand_player player 3 = Distance D200))

let test_nth_hand_player_bad_input =
  Alcotest.test_case "Check if nth_hand_player raise the good exception" `Quick
    (fun () ->
      Alcotest.check_raises "same exception" Index_of_hand_out_of_bound
        (fun () -> ignore (nth_hand_player player_test 17)))

let test_is_card_in_player_hand_true =
  let open QCheck in
  Test.make ~count:1000
    ~name:
      "For all non-empty player hand, for all card in that hand, \
       (is_card_in_player_hand player card) is true" arbitrary_hand (fun hand ->
      assume (hand <> []);
      let player = set_hand_from player_test hand in
      List.for_all (fun card -> is_card_in_player_hand player card) hand)

let test_is_card_in_player_hand_false =
  let open QCheck in
  Test.make ~count:1000
    ~name:
      "For all card, for all hand without card, (is_card_in_player_hand player \
       card) is false" (pair arbitrary_hand arbitrary_card) (fun (hand, card) ->
      assume (not (List.mem card hand));
      let player = set_hand_from player_test hand in
      not (is_card_in_player_hand player card))

let () =
  let open Alcotest in
  run "Teams_engine"
    [
      ( "test have_same_contents_team",
        [ test_have_same_contents_team; test_replace_player_in ] );
      ("test get_names", [ test_get_names ]);
      ( "test get_current_player_from and set_next_player_from",
        [
          test_set_next_player_and_get_current_player1;
          test_set_next_player_and_get_current_player2;
          test_set_next_player1;
          test_set_next_player2;
          test_get_current_player_from;
        ] );
      ( "test_does_player_have_this_name_in_team_list",
        [
          test_does_player_have_this_name_in_team_list1;
          test_does_player_have_this_name_in_team_list2;
          test_does_player_have_this_name_in_team_list3;
        ] );
      ( "test pp_team",
        [
          test_pp_team1;
          test_pp_team2;
          test_pp_team3;
          test_pp_team4;
          test_pp_team5;
        ] );
      ( "test pp_team_with_hand_of",
        [ test_pp_team_with_hand_of1; test_pp_team_with_hand_of2 ] );
      ("test_pp_public_informations_list", [ test_pp_public_informations_list ]);
      ("test_pp_names_of_team_list", [ test_pp_names_of_team_list ]);
      ( "init teams, players and public_informations function tests",
        [ test_init_team_with_one_player; test_init_team_with_two_players ] );
      ( "tests has_safety_to_counter_hazard_on_his_hand",
        [
          test_has_safety_to_counter_hazard_on_his_hand1;
          test_has_safety_to_counter_hazard_on_his_hand2;
          test_has_safety_to_counter_hazard_on_his_hand3;
          test_has_safety_to_counter_hazard_on_his_hand4;
        ] );
      ( "is_usable_hazard_card",
        [
          test_is_usable_hazard_card1;
          test_is_usable_hazard_card2;
          test_is_usable_hazard_card3;
          test_is_usable_hazard_card4;
          test_is_usable_hazard_card5;
        ] );
      ("use_hazard_card", [ test_use_hazard_card ]);
      ( "is_usable_distance_card",
        [
          test_is_usable_distance_card1;
          test_is_usable_distance_card2;
          test_is_usable_distance_card3;
          test_is_usable_distance_card4;
          test_is_usable_distance_card5;
        ] );
      ("use_distance_card", [ test_use_distance_card ]);
      ( "is_usable_safety_card",
        [ test_is_usable_safety_card1; test_is_usable_safety_card2 ] );
      ("use_safety_card", [ test_use_safety_card ]);
      ("use_coup_fouree", [ test_use_coup_fouree ]);
      ( "is_usable_remedy_card",
        [
          test_is_usable_remedy_card1;
          test_is_usable_remedy_card2;
          test_is_usable_remedy_card3;
          test_is_usable_remedy_card4;
          test_is_usable_remedy_card5;
        ] );
      ("use_remedy_card", [ test_use_remedy_card ]);
      ( "test nth_hand_player",
        [ test_nth_hand_player; test_nth_hand_player_bad_input ] );
      ( "test_is_card_in_player_hand_true",
        [ QCheck_alcotest.to_alcotest test_is_card_in_player_hand_true ] );
      ( "test_is_card_in_player_hand_false",
        [ QCheck_alcotest.to_alcotest test_is_card_in_player_hand_false ] );
    ]
