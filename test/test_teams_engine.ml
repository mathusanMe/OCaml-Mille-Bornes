open Mille_bornes.Teams_engine
open Mille_bornes.Cards_engine
open Utils_teams_engine

let test_set_next_player_and_get_current_player1 =
  Alcotest.test_case
    "test set_next_player and get_current_player on team with one computer \
     player"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_player
           (get_current_player_from team_with_one_computer)
           (get_current_player_from
              (set_next_player_from team_with_one_computer))))

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
           (get_current_player_from team_with_computer_human)
           (get_current_player_from
              (set_next_player_from
                 (set_next_player_from team_with_computer_human)))
        && equal_player
             (get_current_player_from
                (set_next_player_from team_with_computer_human))
             (get_current_player_from
                (set_next_player_from
                   (set_next_player_from
                      (set_next_player_from team_with_computer_human))))
        && (not
              (equal_player
                 (get_current_player_from team_with_computer_human)
                 (get_current_player_from
                    (set_next_player_from team_with_computer_human))))
        && not
             (equal_player
                (get_current_player_from
                   (set_next_player_from team_with_computer_human))
                (get_current_player_from
                   (set_next_player_from
                      (set_next_player_from team_with_computer_human))))))

let test_set_next_player1 =
  Alcotest.test_case
    "test if the index of the current player remains at 0 in a team with one \
     player"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (team_with_one_human.current_player_index = 0
        && (set_next_player_from team_with_one_human).current_player_index = 0
        && team_with_one_computer.current_player_index = 0
        && (set_next_player_from team_with_one_computer).current_player_index
           = 0))

let test_set_next_player2 =
  Alcotest.test_case
    "test if the index of the current player changes correctly in a team with \
     2 players"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (team_with_computer_human.current_player_index = 0
        && (set_next_player_from team_with_computer_human).current_player_index
           = 1
        && (set_next_player_from
              (set_next_player_from team_with_computer_human))
             .current_player_index = 0
        && (set_next_player_from
              (set_next_player_from
                 (set_next_player_from team_with_computer_human)))
             .current_player_index = 1))

let test_get_current_player_from =
  Alcotest.test_case "test get_current_player_from" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ((get_player_struct_from
            (get_current_player_from team_with_computer_human))
           .name = "Computer"
        && (get_player_struct_from
              (get_current_player_from
                 (set_next_player_from team_with_computer_human)))
             .name = "Mathusan"))

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
  Alcotest.test_case "test pp_team on team_with_computer_human without hand"
    `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) :\n\
         Computer (computer with strategy strat)\n\
         Mathusan\n\
         Driving Zone : \n\
         Top of speed limit pile : (empty);\n\n\
         Top of drive pile : (empty);\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : (empty);\n\
        \               \n\
         Coup fourree cards : (empty);\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team false) team_with_computer_human))

let test_pp_team2 =
  Alcotest.test_case "test pp_team on team_with_computer_human with hand" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         Computer (computer with strategy strat)\n\
         hand : (empty);\n\
        \       \n\
         Mathusan\n\
         hand : (empty);\n\
        \       \n\
         Driving Zone : \n\
         Top of speed limit pile : (empty);\n\n\
         Top of drive pile : (empty);\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : (empty);\n\
        \               \n\
         Coup fourree cards : (empty);\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team true) team_with_computer_human))

let test_pp_team3 =
  Alcotest.test_case "test pp_team on team_with_one_human and with hand" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         Thomas\n\
         hand : (empty);\n\
        \       \n\
         Driving Zone : \n\
         Top of speed limit pile : (empty);\n\n\
         Top of drive pile : (empty);\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : (empty);\n\
        \               \n\
         Coup fourree cards : (empty);\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team true) team_with_one_human))

let test_pp_team4 =
  Alcotest.test_case "test pp_team on team3 with hand" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         name1\n\
         hand : (empty);\n\
        \       \n\
         name2\n\
         hand : (empty);\n\
        \       \n\
         Driving Zone : \n\
         Top of speed limit pile : Speed limit;\n\n\
         Top of drive pile : Accident;\n\n\
         Distance cards : (empty);\n\
        \                 \n\
         Safety cards : 0. Fuel truck;\n\
        \               \n\
         Coup fourree cards : 0. Emergency vehicle;\n\
        \                     \n\n"
        (Format.asprintf "%a" (pp_team true) team3))

let test_pp_team5 =
  Alcotest.test_case "test pp_team on team6 with hand" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck :\n\
         Thomas\n\
         hand : 0. Repairs;\n\
        \       1. Speed limit;\n\
        \       2. Out of gas;\n\
        \       3. Driving ace;\n\
        \       4. 100;\n\
        \       5. 200;\n\
        \       \n\
         Mathusan\n\
         hand : 0. Drive;\n\
        \       1. Gas;\n\
        \       2. 25;\n\
        \       3. 75;\n\
        \       4. 100;\n\
        \       5. 200;\n\
        \       \n\
         Driving Zone : \n\
         Top of speed limit pile : Speed limit;\n\n\
         Top of drive pile : Drive;\n\n\
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
        (Format.asprintf "%a" (pp_team true) team6))

let test_pp_team_with_hand_of1 =
  Alcotest.test_case "test pp_team_with_hand_of on team3 with hand of name2"
    `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck of name1 :\n\
         name1\n\
         hand : (empty);\n\
        \       \n\
         name2\n\
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
           (pp_team_with_hand_of (List.hd team3.players))
           team3))

let test_pp_team_with_hand_of2 =
  Alcotest.test_case "test pp_team_with_hand_of on team6 with hand of Mathusan"
    `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) with deck of Mathusan :\n\
         Thomas\n\
         Mathusan\n\
         hand : 0. Drive;\n\
        \       1. Gas;\n\
        \       2. 25;\n\
        \       3. 75;\n\
        \       4. 100;\n\
        \       5. 200;\n\
        \       \n\
         Driving Zone : \n\
         Top of speed limit pile : Speed limit;\n\n\
         Top of drive pile : Drive;\n\n\
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
           (pp_team_with_hand_of (List.hd (List.tl team6.players)))
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
        \   Top of drive pile : Out of gas;\n\
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
        \   Top of speed limit pile : Speed limit;\n\
        \   \n\
        \   Top of drive pile : Drive;\n\
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
              (fun t -> t.shared_public_informations)
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
  public_informations.speed_limit_pile = []
  && public_informations.drive_pile = []
  && public_informations.distance_cards = []
  && public_informations.safety_area = []
  && public_informations.coup_fouree_cards = []

let test_init_team_with_one_computer_player =
  Alcotest.test_case "initialisation of a team of 1 computer player " `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length team_with_one_computer.players = 1
        && is_computer (List.nth team_with_one_computer.players 0)
        &&
        let player = List.nth team_with_one_computer.players 0 in
        let player_struct = get_player_struct_from player in
        player_struct.hand = []
        && player_struct.name = "Computer"
        && team_with_one_computer.shared_public_informations.score = 0
        && team_with_one_computer.current_player_index = 0
        && public_informations_is_clear
             team_with_one_computer.shared_public_informations))

let test_init_team_with_one_human_player =
  Alcotest.test_case "initialisation of a team of 1 human player " `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length team_with_one_human.players = 1
        && (not (is_computer (List.nth team_with_one_human.players 0)))
        &&
        let player = List.nth team_with_one_human.players 0 in
        let player_struct = get_player_struct_from player in
        player_struct.hand = []
        && player_struct.name = "Thomas"
        && team_with_one_computer.shared_public_informations.score = 0
        && team_with_one_human.current_player_index = 0
        && public_informations_is_clear
             team_with_one_human.shared_public_informations))

let test_init_team_with_two_computer_players =
  Alcotest.test_case "initialisation of a team of 2 computer players " `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length team_with_two_computers.players = 2
        && is_computer (List.nth team_with_two_computers.players 0)
        && is_computer (List.nth team_with_two_computers.players 1)
        &&
        let player1 = List.nth team_with_two_computers.players 0
        and player2 = List.nth team_with_two_computers.players 1 in
        let player1_struct = get_player_struct_from player1
        and player2_struct = get_player_struct_from player2 in
        player1_struct.hand = [] && player2_struct.hand = []
        && player1_struct.name = "Computer1"
        && player2_struct.name = "Computer2"
        && team_with_two_computers.shared_public_informations.score = 0
        && team_with_two_computers.current_player_index = 0
        && public_informations_is_clear
             team_with_two_computers.shared_public_informations))

let test_init_team_with_two_human_players =
  Alcotest.test_case "initialisation of a team of 2 humans players " `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length team_with_two_humans.players = 2
        && (not (is_computer (List.nth team_with_two_humans.players 0)))
        && (not (is_computer (List.nth team_with_two_humans.players 1)))
        &&
        let player1 = List.nth team_with_two_humans.players 0
        and player2 = List.nth team_with_two_humans.players 1 in
        let player1_struct = get_player_struct_from player1
        and player2_struct = get_player_struct_from player2 in
        player1_struct.hand = [] && player2_struct.hand = []
        && player1_struct.name = "Gabin"
        && player2_struct.name = "Mathusan"
        && team_with_two_humans.shared_public_informations.score = 0
        && team_with_two_humans.current_player_index = 0
        && public_informations_is_clear
             team_with_two_humans.shared_public_informations))

let test_init_team_with_one_computer_player_and_one_human_player =
  Alcotest.test_case
    "initialisation of a team of 1 computer player and 1 human player " `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length team_with_computer_human.players = 2
        && is_computer (List.nth team_with_computer_human.players 0)
        && (not (is_computer (List.nth team_with_computer_human.players 1)))
        &&
        let player1 = List.nth team_with_computer_human.players 0
        and player2 = List.nth team_with_computer_human.players 1 in
        let player1_struct = get_player_struct_from player1
        and player2_struct = get_player_struct_from player2 in
        player1_struct.hand = [] && player2_struct.hand = []
        && player1_struct.name = "Computer"
        && player2_struct.name = "Mathusan"
        && team_with_computer_human.shared_public_informations.score = 0
        && team_with_computer_human.current_player_index = 0
        && public_informations_is_clear
             team_with_computer_human.shared_public_informations))

let test_init_team_with_one_human_player_and_one_computer_player =
  Alcotest.test_case
    "initialisation of a team of 1 human player and 1 computer player " `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (List.length team_with_human_computer.players = 2
        && (not (is_computer (List.nth team_with_human_computer.players 0)))
        && is_computer (List.nth team_with_human_computer.players 1)
        &&
        let player1 = List.nth team_with_human_computer.players 0
        and player2 = List.nth team_with_human_computer.players 1 in
        let player1_struct = get_player_struct_from player1
        and player2_struct = get_player_struct_from player2 in
        player1_struct.hand = [] && player2_struct.hand = []
        && player1_struct.name = "Gabin"
        && player2_struct.name = "Computer"
        && team_with_human_computer.shared_public_informations.score = 0
        && team_with_human_computer.current_player_index = 0
        && public_informations_is_clear
             team_with_human_computer.shared_public_informations))

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
        ([
           Hazard Stop;
           Hazard SpeedLimit;
           Hazard OutOfGas;
           Hazard FlatTire;
           Hazard Accident;
         ]
        |> List.for_all (fun card ->
               not (is_usable_card team1.shared_public_informations card))))

let test_is_usable_hazard_card2 =
  Alcotest.test_case
    "check that a hazard can be used on a team with safety cards and already \
     attacked in without speed_limit_pile"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Hazard SpeedLimit; Hazard OutOfGas ]
         |> List.for_all (fun card ->
                not (is_usable_card team2.shared_public_informations card))
        && [ Hazard Stop; Hazard FlatTire; Hazard Accident ]
           |> List.for_all (fun card ->
                  is_usable_card team2.shared_public_informations card)))

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
               not (is_usable_card team3.shared_public_informations card))))

let test_is_usable_hazard_card4 =
  Alcotest.test_case
    "Test if the EmergencyVehicle card blocks SpeedLimit and Stop cards" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Hazard Stop; Hazard SpeedLimit ]
         |> List.for_all (fun card ->
                not (is_usable_card team4.shared_public_informations card))
        && [ Hazard OutOfGas; Hazard FlatTire; Hazard Accident ]
           |> List.for_all (fun card ->
                  is_usable_card team4.shared_public_informations card)))

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
               is_usable_card team5.shared_public_informations card)))

let test_use_hazard_card =
  Alcotest.test_case "test use_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team1 (Hazard SpeedLimit) in
         let res1 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 speed_limit_pile = [ Hazard SpeedLimit ];
               };
           }
           = team
         in
         let team = use_card team (Hazard SpeedLimit) in
         let res2 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 speed_limit_pile = [ Hazard SpeedLimit; Hazard SpeedLimit ];
               };
           }
           = team
         in
         let team = use_card team (Hazard OutOfGas) in
         let res3 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 speed_limit_pile = [ Hazard SpeedLimit; Hazard SpeedLimit ];
                 drive_pile = [ Hazard OutOfGas ];
               };
           }
           = team
         in
         let team = use_card team (Hazard FlatTire) in
         let res4 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 speed_limit_pile = [ Hazard SpeedLimit; Hazard SpeedLimit ];
                 drive_pile = [ Hazard FlatTire; Hazard OutOfGas ];
               };
           }
           = team
         in
         let team = use_card team (Hazard Accident) in
         let res5 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 speed_limit_pile = [ Hazard SpeedLimit; Hazard SpeedLimit ];
                 drive_pile =
                   [ Hazard Accident; Hazard FlatTire; Hazard OutOfGas ];
               };
           }
           = team
         in
         let team = use_card team (Hazard Stop) in
         let res6 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 speed_limit_pile = [ Hazard SpeedLimit; Hazard SpeedLimit ];
                 drive_pile =
                   [
                     Hazard Stop;
                     Hazard Accident;
                     Hazard FlatTire;
                     Hazard OutOfGas;
                   ];
               };
           }
           = team
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
               not (is_usable_card team1.shared_public_informations card))))

let test_is_usable_distance_card2 =
  Alcotest.test_case "checks if a team has Hazard SpeedLimit" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Distance D25; Distance D50 ]
         |> List.for_all (fun card ->
                is_usable_card team2.shared_public_informations card)
        && [ Distance D75; Distance D100; Distance D200 ]
           |> List.for_all (fun card ->
                  not (is_usable_card team2.shared_public_informations card))))

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
               not (is_usable_card team3.shared_public_informations card))))

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
               is_usable_card team4.shared_public_informations card)))

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
               is_usable_card team5.shared_public_informations card)))

let test_use_distance_card =
  Alcotest.test_case "Test with use_distance_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team5 (Distance D25) in
         let res1 =
           {
             team5 with
             shared_public_informations =
               {
                 team5.shared_public_informations with
                 distance_cards = [ Distance D25 ];
                 score = 25;
               };
           }
           = team
         in
         let team = use_card team (Distance D50) in
         let res2 =
           {
             team5 with
             shared_public_informations =
               {
                 team5.shared_public_informations with
                 distance_cards = [ Distance D25; Distance D50 ];
                 score = 75;
               };
           }
           = team
         in
         let team = use_card team (Distance D75) in
         let res3 =
           {
             team5 with
             shared_public_informations =
               {
                 team5.shared_public_informations with
                 distance_cards = [ Distance D25; Distance D50; Distance D75 ];
                 score = 150;
               };
           }
           = team
         in
         let team = use_card team (Distance D100) in
         let res4 =
           {
             team5 with
             shared_public_informations =
               {
                 team5.shared_public_informations with
                 distance_cards =
                   [ Distance D25; Distance D50; Distance D75; Distance D100 ];
                 score = 250;
               };
           }
           = team
         in
         let team = use_card team (Distance D200) in
         let res5 =
           {
             team5 with
             shared_public_informations =
               {
                 team5.shared_public_informations with
                 distance_cards =
                   [
                     Distance D25;
                     Distance D50;
                     Distance D75;
                     Distance D100;
                     Distance D200;
                   ];
                 score = 450;
               };
           }
           = team
         in
         let team = use_card team (Distance D75) in
         let res6 =
           {
             team5 with
             shared_public_informations =
               {
                 team5.shared_public_informations with
                 distance_cards =
                   [
                     Distance D25;
                     Distance D50;
                     Distance D75;
                     Distance D75;
                     Distance D100;
                     Distance D200;
                   ];
                 score = 525;
               };
           }
           = team
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
               is_usable_card team1.shared_public_informations card)))

let test_is_usable_safety_card2 =
  Alcotest.test_case
    "checks if a team has used a safety_card in safety_area and \
     coup_fouree_cards"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Safety EmergencyVehicle; Safety FuelTruck ]
         |> List.for_all (fun card ->
                not (is_usable_card team3.shared_public_informations card))
        && [ Safety PunctureProof; Safety DrivingAce ]
           |> List.for_all (fun card ->
                  is_usable_card team3.shared_public_informations card)))

let test_use_safety_card =
  Alcotest.test_case "test use_safety_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team1 (Safety FuelTruck) in
         let res1 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 safety_area = [ Safety FuelTruck ];
               };
           }
           = team
         in
         let team = use_card team (Safety EmergencyVehicle) in
         let res2 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 safety_area = [ Safety EmergencyVehicle; Safety FuelTruck ];
               };
           }
           = team
         in
         let team = use_card team (Safety DrivingAce) in
         let res3 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 safety_area =
                   [
                     Safety EmergencyVehicle;
                     Safety FuelTruck;
                     Safety DrivingAce;
                   ];
               };
           }
           = team
         in
         res1 && res2 && res3))

let test_use_coup_fouree =
  Alcotest.test_case "test use_coup_fouree" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_coup_fouree team1 FuelTruck in
         let res1 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 coup_fouree_cards = [ Safety FuelTruck ];
                 score = 200;
               };
           }
           = team
         in
         let team = use_coup_fouree team EmergencyVehicle in
         let res2 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 coup_fouree_cards =
                   [ Safety EmergencyVehicle; Safety FuelTruck ];
                 score = 400;
               };
           }
           = team
         in
         let team = use_coup_fouree team DrivingAce in
         let res3 =
           {
             team1 with
             shared_public_informations =
               {
                 team.shared_public_informations with
                 coup_fouree_cards =
                   [
                     Safety EmergencyVehicle;
                     Safety FuelTruck;
                     Safety DrivingAce;
                   ];
                 score = 600;
               };
           }
           = team
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
                is_usable_card team1.shared_public_informations card)
        && [
             Remedy EndOfSpeedLimit;
             Remedy Gas;
             Remedy SpareTire;
             Remedy Repairs;
           ]
           |> List.for_all (fun card ->
                  not (is_usable_card team1.shared_public_informations card))))

let test_is_usable_remedy_card2 =
  Alcotest.test_case
    "Check if the safety_area blocks hazards so you cannot use a Remedy if the \
     hazard is already countered."
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Remedy EndOfSpeedLimit ]
         |> List.for_all (fun card ->
                is_usable_card team2.shared_public_informations card)
        && [ Remedy Drive; Remedy Gas; Remedy SpareTire; Remedy Repairs ]
           |> List.for_all (fun card ->
                  not (is_usable_card team2.shared_public_informations card))))

let test_is_usable_remedy_card3 =
  Alcotest.test_case
    "Check if the coup_fouree_cards block hazards so you cannot use a Remedy \
     if the hazard is already countered."
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ([ Remedy Repairs ]
         |> List.for_all (fun card ->
                is_usable_card team3.shared_public_informations card)
        && [
             Remedy Drive; Remedy EndOfSpeedLimit; Remedy Gas; Remedy SpareTire;
           ]
           |> List.for_all (fun card ->
                  not (is_usable_card team3.shared_public_informations card))))

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
               not (is_usable_card team4.shared_public_informations card))))

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
               not (is_usable_card team5.shared_public_informations card))))

let test_use_remedy_card =
  Alcotest.test_case "test use_remedy_card" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let team = use_card team1 (Remedy EndOfSpeedLimit) in
         let res1 =
           team.shared_public_informations.speed_limit_pile
           = [ Remedy EndOfSpeedLimit ]
           && {
                team with
                shared_public_informations =
                  { team.shared_public_informations with speed_limit_pile = [] };
              }
              = team1
         in
         let team = use_card team (Remedy EndOfSpeedLimit) in
         let res2 =
           team.shared_public_informations.speed_limit_pile
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && {
                team with
                shared_public_informations =
                  { team.shared_public_informations with speed_limit_pile = [] };
              }
              = team1
         in
         let team = use_card team (Remedy Gas) in
         let res3 =
           team.shared_public_informations.speed_limit_pile
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && team.shared_public_informations.drive_pile = [ Remedy Gas ]
           && {
                team with
                shared_public_informations =
                  {
                    team.shared_public_informations with
                    speed_limit_pile = [];
                    drive_pile = [];
                  };
              }
              = team1
         in
         let team = use_card team (Remedy SpareTire) in
         let res4 =
           team.shared_public_informations.speed_limit_pile
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && team.shared_public_informations.drive_pile
              = [ Remedy SpareTire; Remedy Gas ]
           && {
                team with
                shared_public_informations =
                  {
                    team.shared_public_informations with
                    speed_limit_pile = [];
                    drive_pile = [];
                  };
              }
              = team1
         in
         let team = use_card team (Remedy Repairs) in
         let res5 =
           team.shared_public_informations.speed_limit_pile
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && team.shared_public_informations.drive_pile
              = [ Remedy Repairs; Remedy SpareTire; Remedy Gas ]
           && {
                team with
                shared_public_informations =
                  {
                    team.shared_public_informations with
                    speed_limit_pile = [];
                    drive_pile = [];
                  };
              }
              = team1
         in
         let team = use_card team (Remedy Drive) in
         let res6 =
           team.shared_public_informations.speed_limit_pile
           = [ Remedy EndOfSpeedLimit; Remedy EndOfSpeedLimit ]
           && team.shared_public_informations.drive_pile
              = [ Remedy Drive; Remedy Repairs; Remedy SpareTire; Remedy Gas ]
           && {
                team with
                shared_public_informations =
                  {
                    team.shared_public_informations with
                    speed_limit_pile = [];
                    drive_pile = [];
                  };
              }
              = team1
         in
         res1 && res2 && res3 && res4 && res5 && res6))

let () =
  let open Alcotest in
  run "Teams_engine"
    [
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
        [
          test_init_team_with_one_computer_player;
          test_init_team_with_one_human_player;
          test_init_team_with_two_computer_players;
          test_init_team_with_two_human_players;
          test_init_team_with_one_computer_player_and_one_human_player;
          test_init_team_with_one_human_player_and_one_computer_player;
        ] );
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
    ]
