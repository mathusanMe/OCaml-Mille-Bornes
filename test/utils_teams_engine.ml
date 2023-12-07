open Mille_bornes.Teams_engine
open Mille_bornes.Cards_engine
open Default_strat

let equal_player (player1 : player) (player2 : player) =
  (get_strat_from player1).name = (get_strat_from player2).name
  && get_name_from player1 = get_name_from player2

let teams_with_one_player =
  init_teams [ ("Thomas", strat); ("Computer", strat) ] false

let team_with_one_player = List.nth teams_with_one_player 0
let teams_with_one_player_computer = List.nth teams_with_one_player 1

let teams_with_two_player =
  init_teams
    [
      ("Gabin", strat); ("Mathusan", strat); ("name1", strat); ("name2", strat);
    ]
    true

let team_with_two_players = List.nth teams_with_two_player 0
let team1 = List.nth teams_with_two_player 1

let team2 =
  let team2_temp = team1 in
  let team2_temp = use_card team2_temp (Hazard SpeedLimit) in
  let team2_temp = use_card team2_temp (Hazard OutOfGas) in
  let team2_temp = use_card team2_temp (Remedy Drive) in
  use_card team2_temp (Safety FuelTruck)

let team3 =
  let team3_temp = team1 in
  let team3_temp = use_card team3_temp (Hazard SpeedLimit) in
  let team3_temp = use_card team3_temp (Hazard Stop) in
  let team3_temp = use_card team3_temp (Hazard Accident) in
  let team3_temp = use_card team3_temp (Safety FuelTruck) in
  use_coup_fouree team3_temp EmergencyVehicle

let team4 =
  let team4_temp = team1 in
  let team4_temp = use_card team4_temp (Hazard SpeedLimit) in
  let team4_temp = use_card team4_temp (Hazard Stop) in
  use_coup_fouree team4_temp EmergencyVehicle

let team5 =
  let team5_temp = team1 in

  let team5_temp = use_card team5_temp (Hazard SpeedLimit) in
  let team5_temp = use_card team5_temp (Remedy EndOfSpeedLimit) in
  let team5_temp = use_card team5_temp (Remedy Drive) in
  let team5_temp = use_card team5_temp (Hazard Stop) in
  use_card team5_temp (Remedy Drive)

let team_with_two_players_2 =
  init_teams
    [
      ("Thomas", strat);
      ("Mathusan", strat);
      ("Useless1", strat);
      ("Useless2", strat);
    ]
    true

let team6 =
  let team6_temp = List.nth team_with_two_players_2 0 in
  let team6_temp = use_card team6_temp (Hazard SpeedLimit) in
  let team6_temp = use_card team6_temp (Remedy EndOfSpeedLimit) in
  let team6_temp = use_card team6_temp (Remedy Drive) in
  let team6_temp = use_card team6_temp (Hazard OutOfGas) in
  let team6_temp = use_card team6_temp (Remedy Gas) in
  let team6_temp = use_card team6_temp (Hazard Accident) in
  let team6_temp = use_card team6_temp (Distance D25) in
  let team6_temp = use_card team6_temp (Distance D100) in
  let team6_temp = use_card team6_temp (Distance D100) in
  let team6_temp = use_card team6_temp (Distance D200) in
  let team6_temp = use_card team6_temp (Safety FuelTruck) in
  let team6_temp = use_card team6_temp (Safety DrivingAce) in
  let team6_temp = use_coup_fouree team6_temp EmergencyVehicle in
  let players_team6 = get_players_from team6_temp in
  let player1 = List.nth players_team6 0 in
  let player2 = List.nth players_team6 1 in
  let new_player1 =
    set_hand_from player1
      [
        Remedy Repairs;
        Hazard SpeedLimit;
        Hazard OutOfGas;
        Safety DrivingAce;
        Distance D100;
        Distance D200;
      ]
  in
  let new_player2 =
    set_hand_from player2
      [
        Remedy Drive;
        Remedy Gas;
        Distance D25;
        Distance D75;
        Distance D100;
        Distance D200;
      ]
  in
  let team6_temp = replace_player_in team6_temp new_player1 in
  replace_player_in team6_temp new_player2

let human2 = List.nth (get_players_from team_with_one_player) 0

let human1 =
  set_hand_from human2
    [
      Safety EmergencyVehicle;
      Safety FuelTruck;
      Safety PunctureProof;
      Safety DrivingAce;
    ]

let computer2 = List.nth (get_players_from teams_with_one_player_computer) 0

let computer1 =
  set_hand_from computer2
    [
      Safety EmergencyVehicle;
      Safety FuelTruck;
      Safety PunctureProof;
      Safety DrivingAce;
    ]

let player_test =
  List.nth
    (get_players_from
       (List.nth
          (init_teams [ ("player_name", strat); ("Useless", strat) ] false)
          0))
    0
