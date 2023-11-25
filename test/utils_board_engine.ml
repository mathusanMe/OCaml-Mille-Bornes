open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine

(* Utility Functions for Tests *)

let flatten_teams (teams : team list) =
  teams |> List.map (fun team -> team.players) |> List.flatten

let compare_all_hands_except_player (b1 : board) (b2 : board) (p : player) =
  List.map2
    (fun player new_player -> (player, new_player))
    (flatten_teams b1.teams) (flatten_teams b2.teams)
  |> List.filter (fun (player, new_player) ->
         same_player player new_player && not (same_player player p))
  |> List.map (fun (player, new_player) ->
         (get_player_struct_from player, get_player_struct_from new_player))
  |> List.for_all (fun (player, new_player) ->
         equal_deck_of_card player.hand new_player.hand)

let compare_all_hands_except_two_players (b1 : board) (b2 : board) (p1 : player)
    (p2 : player) =
  List.map2
    (fun player new_player -> (player, new_player))
    (flatten_teams b1.teams) (flatten_teams b2.teams)
  |> List.filter (fun (player, new_player) ->
         same_player player new_player
         && (not (same_player player p1))
         && not (same_player player p2))
  |> List.map (fun (player, new_player) ->
         (get_player_struct_from player, get_player_struct_from new_player))
  |> List.for_all (fun (player, new_player) ->
         equal_deck_of_card player.hand new_player.hand)

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
  let player1 = Human { name = "draw_card_team1_name1"; hand = [] } in
  let player2 = Human { name = "draw_card_team1_name2"; hand = [] } in
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
  let player1 = Human { name = "draw_card_team2_name1"; hand = [] } in
  let player2 = Human { name = "draw_card_team2_name2"; hand = [] } in
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
  let player1 =
    Human { name = "draw_card_team_not_in_board_name1"; hand = [] }
  in
  let player2 =
    Human { name = "draw_card_team_not_in_board_name2"; hand = [] }
  in
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
    Human
      { name = "discard_card_team2_name1"; hand = [ Safety EmergencyVehicle ] }
  in
  let player2 = Human { name = "discard_card_team2_name2"; hand = [] } in
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
    Human
      { name = "discard_card_team1_name1"; hand = [ Safety EmergencyVehicle ] }
  in
  let player2 =
    Human
      { name = "discard_card_team1_name2"; hand = [ Safety EmergencyVehicle ] }
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
  let player1 =
    Human { name = "discard_card_team_not_in_board_name1"; hand = [] }
  in
  let player2 =
    Human { name = "discard_card_team_not_in_board_name2"; hand = [] }
  in
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
  let player1 = Human { name = "place_card_team1_name1"; hand = [] } in
  let player2 = Human { name = "place_card_team1_name2"; hand = [] } in
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
    Human
      {
        name = "place_card_team2_name1";
        hand =
          [ Distance D25; Safety EmergencyVehicle; Remedy Gas; Hazard Accident ];
      }
  in
  let player2 = Human { name = "place_card_team2_name2"; hand = [] } in
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
  let player1 =
    Human { name = "place_card_team_not_in_board_name1"; hand = [] }
  in
  let player2 =
    Human { name = "place_card_team_not_in_board_name2"; hand = [] }
  in
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
  Human { name = "player_team1_name1"; hand = [ Safety EmergencyVehicle ] }

let player12 = Human { name = "player_team1_name2"; hand = [] }

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

let player21 = Human { name = "player_team2_name1"; hand = [] }

let player22 =
  Human { name = "player_team2_name2"; hand = [ Safety FuelTruck ] }

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

let player31 = Human { name = "player_team3_name1"; hand = [] }
let player32 = Human { name = "player_team3_name2"; hand = [] }

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
