open Mille_bornes.Teams_engine

let equal_player (player1 : player) (player2 : player) =
  match (player1, player2) with
  | Computer (p_struct1, p_strat1), Computer (p_struct2, p_strat2) ->
      p_struct1 = p_struct2 && p_strat1.name = p_strat2.name
  | Human p_struct1, Human p_struct2 -> p_struct1 = p_struct2
  | _ -> false

let is_computer = function Computer _ -> true | Human _ -> false

let strat =
  {
    name = "strat";
    choose_card_to_play = (fun _ _ _ -> (0, None));
    want_to_peek_discard_pile = (fun _ _ _ _ -> false);
    want_to_play_coup_fourre = (fun _ _ _ _ -> true);
  }

let team_with_one_computer = init_team_with_one_computer "Computer" strat 7
let team_with_one_human = init_team_with_one_human "Thomas" 8

let team_with_two_computers =
  init_team_with_two_computer "Computer1" strat "Computer2" strat 9

let team_with_two_humans = init_team_with_two_human "Gabin" "Mathusan" 10

let team_with_computer_human =
  init_team_with_one_human_and_one_computer "Computer" true "Mathusan" false
    strat 11

let team_with_human_computer =
  init_team_with_one_human_and_one_computer "Gabin" false "Computer" true strat
    12

open Mille_bornes.Cards_engine

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
      can_drive = false;
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
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
      can_drive = true;
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
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
      can_drive = true;
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
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
      can_drive = true;
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
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
      can_drive = true;
    }
  in
  let player1 = Human { name = "name1"; hand = [] } in
  let player2 = Human { name = "name2"; hand = [] } in
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
      can_drive = true;
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
    shared_public_informations = public_informations;
    current_player_index = 0;
  }

let human1 =
  Human
    {
      name = "Thomas";
      hand =
        [
          Safety EmergencyVehicle;
          Safety FuelTruck;
          Safety PunctureProof;
          Safety DrivingAce;
        ];
    }

let human2 = Human { name = "Thomas"; hand = [] }

let computer1 =
  Computer
    ( {
        name = "Computer";
        hand =
          [
            Safety EmergencyVehicle;
            Safety FuelTruck;
            Safety PunctureProof;
            Safety DrivingAce;
          ];
      },
      strat )

let computer2 = Computer ({ name = "Computer"; hand = [] }, strat)
