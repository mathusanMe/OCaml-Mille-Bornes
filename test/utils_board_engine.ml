open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine
open Default_strat

(* Utility Functions for Tests *)

let flatten_teams (teams : team list) =
  teams |> List.map (fun team -> team.players) |> List.flatten

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

let draw_card_team1 =
  let public_informations =
    {
      id = 1;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "draw_card_team1_name1" strat 0 in
  let player2 = init_player "draw_card_team1_name2" strat 1 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let draw_card_team2 =
  let public_informations =
    {
      id = 2;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "draw_card_team2_name1" strat 2 in
  let player2 = init_player "draw_card_team2_name2" strat 3 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let draw_card_team_not_in_board =
  let public_informations =
    {
      id = 3;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "draw_card_team_not_in_board_name1" strat 5 in
  let player2 = init_player "draw_card_team_not_in_board_name2" strat 6 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

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

let discard_card_team_with_current_player_with_empty_hand =
  let public_informations =
    {
      id = 1;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 =
    set_hand_from
      (init_player "discard_card_team2_name1" strat 7)
      [ Safety EmergencyVehicle ]
  in
  let player2 = init_player "discard_card_team2_name2" strat 8 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 1;
    (* current_player_index = 1 *)
  }

let discard_card_team_with_current_player_with_non_empty_hand =
  let public_informations =
    {
      id = 2;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 =
    set_hand_from
      (init_player "discard_card_team1_name1" strat 9)
      [ Safety EmergencyVehicle ]
  in
  let player2 =
    set_hand_from
      (init_player "discard_card_team1_name2" strat 10)
      [ Safety EmergencyVehicle ]
  in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
    (* current_player_index = 0 *)
  }

let discard_card_team_not_in_board =
  let public_informations =
    {
      id = 3;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "discard_card_team_not_in_board_name1" strat 11 in
  let player2 = init_player "discard_card_team_not_in_board_name2" strat 12 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

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

let place_card_team_with_current_player_with_empty_hand =
  let public_informations =
    {
      id = 1;
      speed_limit_pile = [];
      drive_pile = [ Remedy Drive ];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "place_card_team1_name1" strat 13 in
  let player2 = init_player "place_card_team1_name2" strat 14 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let place_card_team_with_current_player_with_non_empty_hand =
  let public_informations =
    {
      id = 2;
      speed_limit_pile = [];
      drive_pile = [ Remedy Drive ];
      distance_cards = [ Distance D25 ];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 =
    set_hand_from
      (init_player "place_card_team2_name1" strat 15)
      [ Distance D25; Safety EmergencyVehicle; Remedy Gas; Hazard Accident ]
  in

  let player2 = init_player "place_card_team2_name2" strat 16 in

  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let place_card_team_not_in_board =
  let public_informations =
    {
      id = 3;
      speed_limit_pile = [];
      drive_pile = [ Remedy Drive ];
      distance_cards = [ Distance D25 ];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "place_card_team_not_in_board_name1" strat 17 in
  let player2 = init_player "place_card_team_not_in_board_name2" strat 18 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

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

let player11 =
  set_hand_from
    (init_player "player_team1_name1" strat 19)
    [ Safety EmergencyVehicle ]

let player12 = init_player "player_team1_name2" strat 20

let team1 =
  let public_informations =
    {
      id = 0;
      speed_limit_pile = [];
      drive_pile = [ Remedy Drive ];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ player11; player12 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let player21 = init_player "player_team2_name1" strat 21

let player22 =
  set_hand_from (init_player "player_team2_name2" strat 22) [ Safety FuelTruck ]

let team2 =
  let public_informations =
    {
      id = 1;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [ Safety FuelTruck ];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ player21; player22 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let board1 =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ team1; team2 ];
    current_team_index = 0;
  }

let player31 = init_player "player_team3_name1" strat 23
let player32 = init_player "player_team3_name2" strat 24

let team3 =
  let public_informations =
    {
      id = 2;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ player31; player32 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let board2 = { board1 with teams = [ team1; team2; team3 ] }

let board3 =
  {
    board1 with
    draw_pile = generate_initial_pile ();
    teams =
      [
        {
          team3 with
          shared_public_informations = team1.shared_public_informations;
        };
        {
          team3 with
          shared_public_informations = team2.shared_public_informations;
        };
        team3;
      ];
  }

let board4 = { board3 with current_team_index = 3 }
let board5 = { board3 with current_team_index = -1 }

let playerH =
  set_hand_from
    (init_player "Attacker" strat 25)
    [
      Hazard Stop;
      Hazard SpeedLimit;
      Hazard OutOfGas;
      Hazard FlatTire;
      Hazard Accident;
    ]

let board6 =
  {
    draw_pile = [];
    discard_pile = [];
    teams =
      [
        { draw_card_team1 with players = [ playerH ] };
        discard_card_team_with_current_player_with_empty_hand;
      ];
    current_team_index = 0;
  }

let attacker_team =
  let public_informations =
    {
      id = 0;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ playerH ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let schumacher = init_player "Schumacher" strat 26

let team_schumacher =
  let public_informations =
    {
      id = 0;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [ Safety EmergencyVehicle ];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ schumacher ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let board_attack_schumacher =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_schumacher ];
    current_team_index = 0;
  }

let kubica = init_player "Kubica" strat 27

let team_gigakub =
  let public_informations =
    {
      id = 0;
      speed_limit_pile = [];
      drive_pile = [ Remedy Drive ];
      distance_cards = [];
      safety_area = [ Safety DrivingAce ];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ kubica ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let board_attack_kubica =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_gigakub ];
    current_team_index = 0;
  }

let alonso = init_player "Alonso" strat 28

let team_alonso =
  let public_informations =
    {
      id = 0;
      speed_limit_pile = [];
      drive_pile = [ Remedy Drive ];
      distance_cards = [];
      safety_area = [ Safety PunctureProof ];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ alonso ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let board_attack_alonso =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_alonso ];
    current_team_index = 0;
  }

let massa = init_player "Massa" strat 29

let team_massa =
  let public_informations =
    {
      id = 0;
      speed_limit_pile = [];
      drive_pile = [ Remedy Drive ];
      distance_cards = [];
      safety_area = [ Safety FuelTruck ];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  {
    players = [ massa ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let board_attack_massa =
  {
    draw_pile = [];
    discard_pile = [];
    teams = [ attacker_team; team_massa ];
    current_team_index = 0;
  }
