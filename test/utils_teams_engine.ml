open Mille_bornes.Teams_engine
open Default_strat

let equal_player (player1 : player) (player2 : player) =
  (get_strat_from player1).name = (get_strat_from player2).name
  && get_name_from player1 = get_name_from player2

let team_with_one_player = init_team_with_one_player "Thomas" strat 8

let team_with_two_players =
  init_team_with_two_players "Gabin" strat "Mathusan" strat 10

open Mille_bornes.Cards_engine

let team0 =
  let public_informations =
    {
      id = 0;
      speed_limit_pile = [ Hazard SpeedLimit ];
      drive_pile = [ Distance D50; Distance D100 ];
      distance_cards = [ Safety DrivingAce ];
      safety_area = [];
      coup_fouree_cards = [];
      score = 10;
    }
  in
  let player = init_player "player" strat 16 in
  {
    players = [ player ];
    shared_public_informations = public_informations;
    current_player_index = -1;
  }

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
    }
  in
  let player1 = init_player "name1" strat 0 in
  let player2 = init_player "name2" strat 1 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let team2 =
  let public_informations =
    {
      id = 2;
      speed_limit_pile = [ Hazard SpeedLimit ];
      drive_pile = [ Hazard OutOfGas; Remedy Drive ];
      distance_cards = [];
      safety_area = [ Safety FuelTruck ];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "name1" strat 2 in
  let player2 = init_player "name2" strat 3 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let team3 =
  let public_informations =
    {
      id = 3;
      speed_limit_pile = [ Hazard SpeedLimit ];
      drive_pile = [ Hazard Accident; Hazard Stop ];
      distance_cards = [];
      safety_area = [ Safety FuelTruck ];
      coup_fouree_cards = [ Safety EmergencyVehicle ];
      score = 0;
    }
  in
  let player1 = init_player "name1" strat 4 in
  let player2 = init_player "name2" strat 5 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let team4 =
  let public_informations =
    {
      id = 4;
      speed_limit_pile = [ Hazard SpeedLimit ];
      drive_pile = [ Hazard Stop ];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [ Safety EmergencyVehicle ];
      score = 0;
    }
  in
  let player1 = init_player "name1" strat 6 in
  let player2 = init_player "name2" strat 7 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let team5 =
  let public_informations =
    {
      id = 5;
      speed_limit_pile = [ Remedy EndOfSpeedLimit; Hazard SpeedLimit ];
      drive_pile = [ Remedy Drive; Hazard Stop; Remedy Drive ];
      distance_cards = [];
      safety_area = [];
      coup_fouree_cards = [];
      score = 0;
    }
  in
  let player1 = init_player "name1" strat 8 in
  let player2 = init_player "name2" strat 9 in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let team6 =
  let public_informations =
    {
      id = 6;
      speed_limit_pile = [ Hazard SpeedLimit; Remedy EndOfSpeedLimit ];
      drive_pile =
        [ Remedy Drive; Hazard OutOfGas; Remedy Gas; Hazard Accident ];
      distance_cards =
        [ Distance D25; Distance D100; Distance D100; Distance D200 ];
      safety_area = [ Safety FuelTruck; Safety DrivingAce ];
      coup_fouree_cards = [ Safety EmergencyVehicle ];
      score = 0;
    }
  in
  let player1 =
    set_hand_from
      (init_player "Thomas" strat 10)
      [
        Remedy Repairs;
        Hazard SpeedLimit;
        Hazard OutOfGas;
        Safety DrivingAce;
        Distance D100;
        Distance D200;
      ]
  in
  let player2 =
    set_hand_from
      (init_player "Mathusan" strat 11)
      [
        Remedy Drive;
        Remedy Gas;
        Distance D25;
        Distance D75;
        Distance D100;
        Distance D200;
      ]
  in
  {
    players = [ player1; player2 ];
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let human1 =
  set_hand_from
    (init_player "Thomas" strat 12)
    [
      Safety EmergencyVehicle;
      Safety FuelTruck;
      Safety PunctureProof;
      Safety DrivingAce;
    ]

let human2 = init_player "Thomas" strat 13

let computer1 =
  set_hand_from
    (init_player "Computer" strat 14)
    [
      Safety EmergencyVehicle;
      Safety FuelTruck;
      Safety PunctureProof;
      Safety DrivingAce;
    ]

let computer2 = init_player "Computer" strat 15
