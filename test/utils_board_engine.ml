open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine
open Default_strat

(* Utility Functions for Tests *)

let flatten_teams (teams : team list) =
  teams |> List.map (fun team -> get_players_from team) |> List.flatten

let compare_all_hands_except_player (b1 : board) (b2 : board) (p : player) =
  List.map2
    (fun player new_player -> (player, new_player))
    (flatten_teams b1.teams) (flatten_teams b2.teams)
  |> List.filter (fun (player, new_player) ->
         have_same_id_player player new_player
         && not (have_same_id_player player p))
  |> List.for_all (fun (player, new_player) ->
         equal_deck_of_card (get_hand_from player) (get_hand_from new_player))

let compare_all_hands_except_two_players (b1 : board) (b2 : board) (p1 : player)
    (p2 : player) =
  List.map2
    (fun player new_player -> (player, new_player))
    (flatten_teams b1.teams) (flatten_teams b2.teams)
  |> List.filter (fun (player, new_player) ->
         have_same_id_player player new_player
         && (not (have_same_id_player player p1))
         && not (have_same_id_player player p2))
  |> List.for_all (fun (player, new_player) ->
         equal_deck_of_card (get_hand_from player) (get_hand_from new_player))

(* Test examples *)

let draw_card_teams =
  init_teams
    [
      ("draw_card_team1_name1", strat);
      ("draw_card_team1_name2", strat);
      ("draw_card_team2_name1", strat);
      ("draw_card_team2_name2", strat);
      ("draw_card_team_not_in_board_name1", strat);
      ("draw_card_team_not_in_board_name2", strat);
    ]
    true

let draw_card_team1 = List.nth draw_card_teams 0
let draw_card_team2 = List.nth draw_card_teams 1
let draw_card_team_not_in_board = List.nth draw_card_teams 2

let draw_card_board_with_draw_pile =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ draw_card_team1; draw_card_team2 ];
    current_team_index = 1;
  }

let draw_card_board_with_empty_draw_pile =
  {
    draw_pile = [];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ draw_card_team1; draw_card_team2 ];
    current_team_index = 1;
  }

let draw_card_board_with_empty_discard_pile =
  {
    draw_pile = [ Hazard Accident; Remedy Repairs; Distance D25 ];
    discard_pile = [];
    teams = [ draw_card_team1; draw_card_team2 ];
    current_team_index = 1;
  }

let board_with_empty_draw_pile_and_heavy_discard_pile =
  {
    draw_pile = [];
    discard_pile = generate_initial_pile ();
    teams = [ draw_card_team1; draw_card_team2 ];
    current_team_index = 1;
  }

let discard_card_teams =
  init_teams
    [
      ("discard_card_team1_name1", strat);
      ("discard_card_team1_name2", strat);
      ("discard_card_team2_name1", strat);
      ("discard_card_team2_name2", strat);
      ("discard_card_team_not_in_board_name1", strat);
      ("discard_card_team_not_in_board_name2", strat);
    ]
    true

let discard_card_team1 = List.nth discard_card_teams 0

let discard_card_team_with_current_player_with_empty_hand =
  let players = get_players_from discard_card_team1 in
  let player1 = List.nth players 0 in
  let new_player1 = set_hand_from player1 [ Safety EmergencyVehicle ] in
  let new_team = replace_player_in discard_card_team1 new_player1 in
  set_next_player_from new_team

let discard_card_team_with_current_player_with_non_empty_hand =
  let players = get_players_from discard_card_team1 in
  let player1 = List.nth players 0 in
  let new_player1 = set_hand_from player1 [ Safety EmergencyVehicle ] in
  let player2 = List.nth players 1 in
  let new_player2 = set_hand_from player2 [ Safety EmergencyVehicle ] in
  let new_team = replace_player_in discard_card_team1 new_player1 in
  let new_team = replace_player_in new_team new_player2 in
  new_team

let discard_card_team_not_in_board = List.nth discard_card_teams 2

let discard_card_board_with_team_with_current_player_with_empty_hand =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ discard_card_team_with_current_player_with_empty_hand ];
    current_team_index = 0;
  }

let discard_card_board_with_team_with_current_player_with_non_empty_hand =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ discard_card_team_with_current_player_with_non_empty_hand ];
    current_team_index = 0;
  }

let place_card_teams =
  init_teams
    [
      ("place_card_team1_name1", strat);
      ("place_card_team1_name2", strat);
      ("place_card_team2_name1", strat);
      ("place_card_team2_name2", strat);
      ("place_card_team_not_in_board_name1", strat);
      ("place_card_team_not_in_board_name2", strat);
    ]
    true

let place_card_team_with_current_player_with_empty_hand =
  let team1 = List.nth place_card_teams 0 in
  use_card team1 (Remedy Drive)

let place_card_team_with_current_player_with_non_empty_hand =
  let team2 = List.nth place_card_teams 1 in
  let new_team2 = use_card team2 (Remedy Drive) in
  let new_team2 = use_card new_team2 (Distance D25) in
  let player1 = List.nth (get_players_from new_team2) 0 in
  let new_player1 =
    set_hand_from player1
      [ Distance D25; Safety EmergencyVehicle; Remedy Gas; Hazard Accident ]
  in
  replace_player_in new_team2 new_player1

let place_card_team_not_in_board =
  let team3 = List.nth place_card_teams 2 in
  let new_team3 = use_card team3 (Remedy Drive) in
  let new_team3 = use_card new_team3 (Distance D25) in
  new_team3

let place_card_board_with_team_with_current_player_with_empty_hand =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ place_card_team_with_current_player_with_empty_hand ];
    current_team_index = 0;
  }

let place_card_board_with_team_with_current_player_with_non_empty_hand =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ place_card_team_with_current_player_with_non_empty_hand ];
    current_team_index = 0;
  }

let place_card_board_with_teams =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams =
      [
        place_card_team_with_current_player_with_non_empty_hand;
        place_card_team_with_current_player_with_empty_hand;
      ];
    current_team_index = 0;
  }

let player_teams =
  init_teams
    [
      ("player_team1_name1", strat);
      ("player_team1_name2", strat);
      ("player_team2_name1", strat);
      ("player_team2_name2", strat);
      ("player_team3_name1", strat);
      ("player_team3_name2", strat);
    ]
    true

let team1 =
  let new_team = List.nth player_teams 0 in
  let new_team = use_card new_team (Remedy Drive) in
  let players = get_players_from new_team in
  let player11 = List.nth players 0 in
  let new_player11 = set_hand_from player11 [ Safety EmergencyVehicle ] in
  replace_player_in new_team new_player11

let player11 =
  let players = get_players_from team1 in
  List.nth players 0

let player12 =
  let players = get_players_from team1 in
  List.nth players 1

let team2 =
  let new_team = List.nth player_teams 1 in
  let new_team = use_card new_team (Safety FuelTruck) in
  let players = get_players_from new_team in
  let player22 = List.nth players 1 in
  let new_player22 = set_hand_from player22 [ Safety FuelTruck ] in
  replace_player_in new_team new_player22

let player21 =
  let players = get_players_from team2 in
  List.nth players 0

let player22 =
  let players = get_players_from team2 in
  List.nth players 1

let board1 =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ team1; team2 ];
    current_team_index = 0;
  }

let team3 = List.nth player_teams 2

let player31 =
  let players = get_players_from team3 in
  List.nth players 0

let player32 =
  let players = get_players_from team3 in
  List.nth players 1

let board2 = { board1 with teams = [ team1; team2; team3 ] }

let attack_schumacher_teams =
  init_teams [ ("Attacker", strat); ("Schumacher", strat) ] false

let attacker_team =
  let new_attacker_team = List.nth attack_schumacher_teams 0 in
  let player1 = List.nth (get_players_from new_attacker_team) 0 in
  let new_player1 =
    set_hand_from player1
      [
        Hazard Stop;
        Hazard SpeedLimit;
        Hazard OutOfGas;
        Hazard FlatTire;
        Hazard Accident;
      ]
  in
  replace_player_in new_attacker_team new_player1

let playerH = List.nth (get_players_from attacker_team) 0

let team_schumacher =
  let new_team_schumacher = List.nth attack_schumacher_teams 1 in
  use_card new_team_schumacher (Safety EmergencyVehicle)

let schumacher = List.nth (get_players_from team_schumacher) 0

let board_attack_schumacher =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_schumacher ];
    current_team_index = 0;
  }

let kubica_teams = init_teams [ ("Unused", strat); ("Kubica", strat) ] false

let team_gigakub =
  let new_team_gigakub = List.nth kubica_teams 1 in
  let new_team_gigakub = use_card new_team_gigakub (Remedy Drive) in
  use_card new_team_gigakub (Safety DrivingAce)

let kubica = List.nth (get_players_from team_gigakub) 0

let board_attack_kubica =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_gigakub ];
    current_team_index = 0;
  }

let alonso_teams = init_teams [ ("Unused", strat); ("Alonso", strat) ] false

let team_alonso =
  let new_team_alonso = List.nth alonso_teams 1 in
  let new_team_alonso = use_card new_team_alonso (Remedy Drive) in
  use_card new_team_alonso (Safety PunctureProof)

let alonso = List.nth (get_players_from team_alonso) 0

let board_attack_alonso =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_alonso ];
    current_team_index = 0;
  }

let massa_teams = init_teams [ ("Unused", strat); ("Massa", strat) ] false

let team_massa =
  let new_team_massa = List.nth massa_teams 1 in
  let new_team_massa = use_card new_team_massa (Remedy Drive) in
  use_card new_team_massa (Safety FuelTruck)

let massa = List.nth (get_players_from team_massa) 0

let board_attack_massa =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_massa ];
    current_team_index = 0;
  }
