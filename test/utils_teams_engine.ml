open Mille_bornes.Teams_engine

let team_with_one_computer = init_team_with_one_player "Computer" true
let team_with_one_human = init_team_with_one_player "Thomas" false

let team_with_two_computers =
  init_team_with_two_players "Computer1" true "Computer2" true

let team_with_two_humans =
  init_team_with_two_players "Gabin" false "Thomas" false

let team_with_computer_human =
  init_team_with_two_players "Computer" true "Gabin" false

let team_with_human_computer =
  init_team_with_two_players "Gabin" false "Computer" true

open Mille_bornes.Cards_engine

let team1 =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    score = 0;
    can_drive = true;
    current_player_index = 0;
  }

let team2 =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [ Safety FuelTruck ];
      coup_fouree_cards = [];
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    score = 0;
    can_drive = true;
    current_player_index = 0;
  }

let team3 =
  let driving_zone =
    {
      speed_limit_pile = [];
      drive_pile = [];
      distance_cards = [];
      safety_area = [ Safety FuelTruck ];
      coup_fouree_cards = [ Safety EmergencyVehicle ];
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    score = 0;
    can_drive = true;
    current_player_index = 0;
  }
