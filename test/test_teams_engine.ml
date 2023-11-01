open Mille_bornes.Teams_engine
open Utils_teams_engine

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
        && player2_struct.name = "Thomas"
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
        && player2_struct.name = "Gabin"
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

let () =
  let open Alcotest in
  run "Teams_engine"
    [
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
    ]
