open Mille_bornes.Teams_engine

let team_with_one_computer = init_team_with_one_player "Computer" true
let team_with_one_human = init_team_with_one_player "Thomas" false

let team_with_two_computers =
  init_team_with_two_players "Computer1" true "Computer2" true

let team_with_two_humans =
  init_team_with_two_players "Gabin" false "Mathusan" false

let team_with_computer_human =
  init_team_with_two_players "Computer" true "Mathusan" false

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
    can_drive = false;
    current_player_index = 0;
  }

let team2 =
  let driving_zone =
    {
      speed_limit_pile = [ Hazard SpeedLimit ];
      drive_pile = [ Hazard OutOfGas; Remedy Drive ];
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
      speed_limit_pile = [ Hazard SpeedLimit ];
      drive_pile = [ Hazard Accident; Hazard Stop ];
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

let team4 =
  let driving_zone =
    {
      speed_limit_pile = [ Hazard SpeedLimit ];
      drive_pile = [ Hazard Stop ];
      distance_cards = [];
      safety_area = [];
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

let team5 =
  let driving_zone =
    {
      speed_limit_pile = [ Remedy EndOfSpeedLimit; Hazard SpeedLimit ];
      drive_pile = [ Remedy Drive; Hazard Stop; Remedy Drive ];
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

let team6 =
  let driving_zone =
    {
      speed_limit_pile = [ Hazard SpeedLimit; Remedy EndOfSpeedLimit ];
      drive_pile =
        [ Remedy Drive; Hazard OutOfGas; Remedy Gas; Hazard Accident ];
      distance_cards =
        [ Distance D25; Distance D100; Distance D100; Distance D200 ];
      safety_area = [ Safety FuelTruck; Safety DrivingAce ];
      coup_fouree_cards = [ Safety EmergencyVehicle ];
    }
  in
  let player1 =
    Human
      {
        name = "Thomas";
        hand =
          [
            Remedy Repairs;
            Hazard SpeedLimit;
            Hazard OutOfGas;
            Safety DrivingAce;
            Distance D100;
            Distance D200;
          ];
      }
  in
  let player2 =
    Human
      {
        name = "Mathusan";
        hand =
          [
            Remedy Drive;
            Remedy Gas;
            Distance D25;
            Distance D75;
            Distance D100;
            Distance D200;
          ];
      }
  in
  {
    players = [ player1; player2 ];
    shared_driving_zone = driving_zone;
    score = 0;
    can_drive = true;
    current_player_index = 0;
  }
