open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine

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
      can_drive = true;
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
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
      can_drive = true;
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
      id = 2;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
      can_drive = true;
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

let board_with_empty_draw_pile_and_heavy_discard_pile =
  {
    draw_pile = [];
    discard_pile = generate_initial_pile ();
    teams = [ team1; team2 ];
    current_team_index = 1;
let discard_card_team_with_current_player_with_empty_hand =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
    }
  in
  let player1 =
    Human
      { name = "discard_card_team2_name1"; hand = [ Safety EmergencyVehicle ] }
  in
  let player2 = Human { name = "discard_card_team2_name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    score = 0;
    can_drive = true;
    current_player_index = 1;
    (* current_player_index = 1 *)
  }

let discard_card_team_with_current_player_with_non_empty_hand =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
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
    shared_driving_zone = driving_zone;
    score = 0;
    can_drive = true;
    current_player_index = 0;
    (* current_player_index = 0 *)
  }

let discard_card_team_not_in_board =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
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
    shared_driving_zone = driving_zone;
    score = 0;
    can_drive = true;
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
