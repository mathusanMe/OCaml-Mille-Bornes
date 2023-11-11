open Mille_bornes.Board_engine
open Mille_bornes.Cards_engine
open Mille_bornes.Teams_engine

let team1 =
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
  let player1 = Human { name = "team1_name1"; hand = [] } in
  let player2 = Human { name = "team1_name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let team2 =
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
  let player1 = Human { name = "team2_name1"; hand = [] } in
  let player2 = Human { name = "team2_name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let team_not_in_board =
  let public_informations =
    {
      id = 3;
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
      can_drive = true;
    }
  in
  let player1 = Human { name = "team_not_in_board_name1"; hand = [] } in
  let player2 = Human { name = "team_not_in_board_name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
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

let board_with_empty_draw_pile_and_heavy_discard_pile =
  {
    draw_pile = [];
    discard_pile = generate_initial_pile ();
    teams = [ team1; team2 ];
    current_team_index = 1;
  }
