open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine

let team1 =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      can_drive = true;
      distance_cards = [];
      score = 0;
      safety_area = [];
      coup_fouree_cards = [];
    }
  in
  let player1 = Human { name = "team1_name1"; hand = [] } in
  let player2 = Human { name = "team1_name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    current_player_index = 0;
  }

let team2 =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      can_drive = true;
      distance_cards = [];
      score = 0;
      safety_area = [];
      coup_fouree_cards = [];
    }
  in
  let player1 = Human { name = "team2_name1"; hand = [] } in
  let player2 = Human { name = "team2_name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    current_player_index = 0;
  }

let team_not_in_board =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      can_drive = true;
      distance_cards = [];
      score = 0;
      safety_area = [];
      coup_fouree_cards = [];
    }
  in
  let player1 = Human { name = "team_not_in_board_name1"; hand = [] } in
  let player2 = Human { name = "team_not_in_board_name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    current_player_index = 0;
  }

let board_with_draw_pile =
  {
    draw_pile = [ Distance D200; Remedy Drive ];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ team1; team2 ];
    current_team_index = 1;
  }

let board_with_empty_draw_pile =
  {
    draw_pile = [];
    discard_pile = [ Hazard Accident; Remedy Repairs ];
    teams = [ team1; team2 ];
    current_team_index = 1;
  }
