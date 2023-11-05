open Mille_bornes.Teams_engine
open Utils_teams_engine

let test_set_next_player_and_get_current_player1 =
  Alcotest.test_case
    "test set_next_player and get_current_player on team with one computer \
     player"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_current_player_from team_with_one_computer
        = get_current_player_from (set_next_player_from team_with_one_computer)
        ))

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
        (get_current_player_from team_with_computer_human
         = get_current_player_from
             (set_next_player_from
                (set_next_player_from team_with_computer_human))
        && get_current_player_from
             (set_next_player_from team_with_computer_human)
           = get_current_player_from
               (set_next_player_from
                  (set_next_player_from
                     (set_next_player_from team_with_computer_human)))
        && get_current_player_from team_with_computer_human
           != get_current_player_from
                (set_next_player_from team_with_computer_human)
        && get_current_player_from
             (set_next_player_from team_with_computer_human)
           != get_current_player_from
                (set_next_player_from
                   (set_next_player_from team_with_computer_human))))

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

let test_pp_team1 =
  Alcotest.test_case "test pp_team on team_with_computer_human without hand"
    `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Name(s) :\n\
         Computer (computer)\n\
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
         Computer (computer)\n\
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

let driving_zone_is_clear driving_zone =
  driving_zone.speed_limit_pile = []
  && driving_zone.drive_pile = []
  && driving_zone.distance_cards = []
  && driving_zone.safety_area = []
  && driving_zone.coup_fouree_cards = []

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
        && team_with_one_computer.score = 0
        && team_with_one_computer.can_drive = false
        && team_with_one_computer.current_player_index = 0
        && driving_zone_is_clear team_with_one_computer.shared_driving_zone))

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
        && team_with_one_computer.score = 0
        && team_with_one_computer.can_drive = false
        && team_with_one_human.current_player_index = 0
        && driving_zone_is_clear team_with_one_human.shared_driving_zone))

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
        && team_with_two_computers.score = 0
        && team_with_two_computers.can_drive = false
        && team_with_two_computers.current_player_index = 0
        && driving_zone_is_clear team_with_two_computers.shared_driving_zone))

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
        && team_with_two_humans.score = 0
        && team_with_two_humans.can_drive = false
        && team_with_two_humans.current_player_index = 0
        && driving_zone_is_clear team_with_two_humans.shared_driving_zone))

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
        && team_with_computer_human.score = 0
        && team_with_computer_human.can_drive = false
        && team_with_computer_human.current_player_index = 0
        && driving_zone_is_clear team_with_computer_human.shared_driving_zone))

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
        && team_with_human_computer.score = 0
        && team_with_human_computer.can_drive = false
        && team_with_human_computer.current_player_index = 0
        && driving_zone_is_clear team_with_human_computer.shared_driving_zone))

let test_has_already_used_safety1 =
  Alcotest.test_case "given safety card not used by team yet, can be used"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ((not (has_already_used_safety_card team1 FuelTruck))
        && (not (has_already_used_safety_card team1 EmergencyVehicle))
        && (not (has_already_used_safety_card team1 PunctureProof))
        && not (has_already_used_safety_card team1 DrivingAce)))

let test_has_already_used_safety2 =
  Alcotest.test_case "given safety card used by team, can't be reused" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (has_already_used_safety_card team2 FuelTruck
        && (not (has_already_used_safety_card team2 EmergencyVehicle))
        && (not (has_already_used_safety_card team2 PunctureProof))
        && not (has_already_used_safety_card team2 DrivingAce)))

let test_has_already_used_safety3 =
  Alcotest.test_case
    "given safety card used as coup fourre by team, can't be reused" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (has_already_used_safety_card team3 FuelTruck
        && has_already_used_safety_card team3 EmergencyVehicle
        && (not (has_already_used_safety_card team3 PunctureProof))
        && not (has_already_used_safety_card team3 DrivingAce)))

let test_is_attacked1 =
  Alcotest.test_case
    "check if a team is attacked neither on its speed_limite_pile nor on its \
     drive_pile"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ((not (is_attacked_by_hazard_on_drive_pile team1))
        && not (is_attacked_by_speed_limit team1)))

let test_is_attacked2 =
  Alcotest.test_case
    "checks if a team is attacked on its speed_limite_pile but not on its \
     drive_pile"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ((not (is_attacked_by_hazard_on_drive_pile team2))
        && is_attacked_by_speed_limit team2))

let test_is_attacked3 =
  Alcotest.test_case
    "checks if a team is attacked on its drive_pile but not on its \
     speed_limite_pile"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (is_attacked_by_hazard_on_drive_pile team3
        && not (is_attacked_by_speed_limit team3)))

let test_is_attacked4 =
  Alcotest.test_case
    "Test if the EmergencyVehicle card blocks SpeedLimit and Stop cards" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        ((not (is_attacked_by_hazard_on_drive_pile team4))
        && not (is_attacked_by_speed_limit team4)))

let test_is_attacked5 =
  Alcotest.test_case "Test if the remedy block hazard" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        ((not (is_attacked_by_hazard_on_drive_pile team5))
        && not (is_attacked_by_speed_limit team5)))

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
      ( "test pp_team",
        [
          test_pp_team1;
          test_pp_team2;
          test_pp_team3;
          test_pp_team4;
          test_pp_team5;
        ] );
      ( "init teams, players and driving_zone function tests",
        [
          test_init_team_with_one_computer_player;
          test_init_team_with_one_human_player;
          test_init_team_with_two_computer_players;
          test_init_team_with_two_human_players;
          test_init_team_with_one_computer_player_and_one_human_player;
          test_init_team_with_one_human_player_and_one_computer_player;
        ] );
      ( "has_already_used_safety_card",
        [
          test_has_already_used_safety1;
          test_has_already_used_safety2;
          test_has_already_used_safety3;
        ] );
      ( "is attacked on drive pile or on speed limit pile",
        [
          test_is_attacked1;
          test_is_attacked2;
          test_is_attacked3;
          test_is_attacked4;
          test_is_attacked5;
        ] );
    ]
