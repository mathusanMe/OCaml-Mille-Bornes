open Mille_bornes.Board_engine
open Mille_bornes.Teams_engine
open Mille_bornes.Cards_engine
open Utils_board_engine
open Utils_cards_engine

let test_switch_current_player_of_current_team_from =
  Alcotest.test_case "test switch_current_player_of_current_team_from" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let board2_with_idx_of_current_team_changed1 =
           switch_current_player_of_current_team_from board2
         in
         let board2_with_idx_of_current_team_changed0 =
           switch_current_player_of_current_team_from
             board2_with_idx_of_current_team_changed1
         in
         let current_team_initital = get_current_team_from board2 in
         let current_team_1 =
           get_current_team_from board2_with_idx_of_current_team_changed1
         in
         let current_team_0 =
           get_current_team_from board2_with_idx_of_current_team_changed0
         in
         get_current_player_id_from current_team_initital = 0
         && get_current_player_id_from current_team_1 = 1
         && get_current_player_id_from current_team_0 = 0))

let test_switch_current_team_from =
  Alcotest.test_case "test switch_current_team_from on board2" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let board2_with_idx1 = switch_current_team_from board2 in
         let board2_with_idx2 = switch_current_team_from board2_with_idx1 in
         let board2_with_idx_return_to_0 =
           switch_current_team_from board2_with_idx2
         in
         board2.current_team_index = 0
         && board2_with_idx1.current_team_index = 1
         && board2_with_idx2.current_team_index = 2
         && board2_with_idx_return_to_0.current_team_index = 0))

let test_draw_card_from_draw_pile_for_team_not_in_game =
  Alcotest.test_case
    "raise Team_not_found on draw from draw pile for team not in game" `Quick
    (fun () ->
      Alcotest.check_raises "Expected Team_not_found" Team_not_found (fun () ->
          ignore
            (draw_card draw_card_board_with_draw_pile
               draw_card_team_not_in_board false)))

let test_draw_card_from_non_empty_draw_pile =
  Alcotest.test_case "draw card from non empty draw pile" `Quick (fun () ->
      Alcotest.(check bool)
        "draw and add card to current team's current player's hand" true
        (let draw_pile = draw_card_board_with_draw_pile.draw_pile in
         let card = peek_card_from_pile draw_pile in
         let current_team =
           get_current_team_from draw_card_board_with_draw_pile
         in
         let current_player = get_current_player_from current_team in
         let current_player_hand = get_hand_from current_player in
         let new_board_with_draw_pile =
           draw_card draw_card_board_with_draw_pile current_team false
         in
         compare_all_hands_except_player draw_card_board_with_draw_pile
           new_board_with_draw_pile current_player
         &&
         let new_team =
           List.find
             (fun team -> same_team team current_team)
             new_board_with_draw_pile.teams
         in
         let new_player =
           List.find
             (fun player -> have_same_id_player player current_player)
             (get_players_from new_team)
         in
         let new_hand = get_hand_from new_player in
         List.mem card new_hand
         && List.length new_hand = List.length current_player_hand + 1
         && List.length draw_card_board_with_draw_pile.draw_pile
            = List.length new_board_with_draw_pile.draw_pile + 1))

let test_draw_card_from_empty_discard_pile =
  Alcotest.test_case "raise Empty_pile on draw from empty discard pile" `Quick
    (fun () ->
      Alcotest.check_raises "Expected Empty_pile" Empty_pile (fun () ->
          ignore
            (draw_card draw_card_board_with_empty_discard_pile
               (get_current_team_from draw_card_board_with_empty_discard_pile)
               true)))

let test_draw_card_from_discard_pile_for_team_not_in_game =
  Alcotest.test_case
    "raise Team_not_found on draw from discard pile for team not in game" `Quick
    (fun () ->
      Alcotest.check_raises "Expected Team_not_found" Team_not_found (fun () ->
          ignore
            (draw_card board_with_empty_draw_pile_and_heavy_discard_pile
               draw_card_team_not_in_board true)))

let test_draw_card_from_non_empty_discard_pile =
  Alcotest.test_case "draw card from non empty discard pile" `Quick (fun () ->
      Alcotest.(check bool)
        "draw and add card to current team's current player's hand" true
        (let discard_pile =
           board_with_empty_draw_pile_and_heavy_discard_pile.discard_pile
         in
         let card = peek_card_from_pile discard_pile in
         let current_team =
           get_current_team_from
             board_with_empty_draw_pile_and_heavy_discard_pile
         in
         let current_player = get_current_player_from current_team in
         let current_player_hand = get_hand_from current_player in
         let new_board_with_discard_pile =
           draw_card board_with_empty_draw_pile_and_heavy_discard_pile
             current_team true
         in
         compare_all_hands_except_player
           board_with_empty_draw_pile_and_heavy_discard_pile
           new_board_with_discard_pile current_player
         &&
         let new_team =
           List.find
             (fun team -> same_team team current_team)
             new_board_with_discard_pile.teams
         in
         let new_player =
           List.find
             (fun player -> have_same_id_player player current_player)
             (get_players_from new_team)
         in
         let new_hand = get_hand_from new_player in
         List.mem card new_hand
         && List.length new_hand = List.length current_player_hand + 1
         && List.length
              board_with_empty_draw_pile_and_heavy_discard_pile.discard_pile
            = List.length new_board_with_discard_pile.discard_pile + 1))

let test_switch_draw_and_discard_pile_from_empty_draw_pile =
  Alcotest.test_case "switch empty draw pile and discard pile" `Quick (fun () ->
      Alcotest.(check bool)
        "test if the discard_pile of new board and draw pile of the tested \
         board are the same and the draw pile of the new board and the discard \
         pile of the tested board are the same on content"
        true
        (let new_board =
           swap_draw_and_shuffled_discard_pile
             draw_card_board_with_empty_draw_pile
         in
         is_discard_pile_empty new_board
         && equal_pile_of_card
              (sort_card_list new_board.draw_pile)
              (sort_card_list draw_card_board_with_empty_draw_pile.discard_pile)))

let test_switch_draw_and_discard_pile_from_empty_draw_pile_and_heavy_discard_pile
    =
  Alcotest.test_case "switch empty draw pile and discard pile from board" `Quick
    (fun () ->
      Alcotest.(check bool)
        "test if the discard_pile of new board and draw pile of the tested \
         board are the same and the draw pile of the new board and the discard \
         pile of the tested board are the same on content"
        true
        (let new_board =
           swap_draw_and_shuffled_discard_pile
             board_with_empty_draw_pile_and_heavy_discard_pile
         in
         is_discard_pile_empty new_board
         && equal_pile_of_card
              (sort_card_list new_board.draw_pile)
              (sort_card_list
                 board_with_empty_draw_pile_and_heavy_discard_pile.discard_pile)))

let test_discard_card_from_team_not_in_board =
  Alcotest.test_case
    "raise Team_not_found on discard card from team not in board" `Quick
    (fun () ->
      Alcotest.check_raises "Expected Team_not_found" Team_not_found (fun () ->
          ignore
            (discard_card
               discard_card_board_with_team_with_current_player_with_empty_hand
               discard_card_team_not_in_board (Safety EmergencyVehicle))))

let test_discard_card_from_team_with_current_player_with_empty_hand =
  Alcotest.test_case
    "raise Empty_deck on discard card from team with current player with empty \
     hand"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Empty_deck" Empty_deck (fun () ->
          ignore
            (discard_card
               discard_card_board_with_team_with_current_player_with_empty_hand
               discard_card_team_with_current_player_with_empty_hand
               (Safety EmergencyVehicle))))

let test_discard_card_from_team_with_current_player_without_card_within_hand =
  Alcotest.test_case
    "raise Card_not_found on discard card from team with current player \
     without card within hand"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Card_not_found" Card_not_found (fun () ->
          ignore
            (discard_card
               discard_card_board_with_team_with_current_player_with_non_empty_hand
               discard_card_team_with_current_player_with_non_empty_hand
               (Hazard OutOfGas))))

let test_discard_card_from_team_with_current_player_with_card_within_hand =
  Alcotest.test_case
    "discard card from team with current player with card within hand" `Quick
    (fun () ->
      Alcotest.(check bool)
        "discard card from team with current player with card within hand" true
        (let current_team =
           get_current_team_from
             discard_card_board_with_team_with_current_player_with_non_empty_hand
         in
         let current_player = get_current_player_from current_team in
         let current_player_hand = get_hand_from current_player in
         let new_board_with_discard_pile =
           discard_card
             discard_card_board_with_team_with_current_player_with_non_empty_hand
             current_team (Safety EmergencyVehicle)
         in
         compare_all_hands_except_player
           discard_card_board_with_team_with_current_player_with_non_empty_hand
           new_board_with_discard_pile current_player
         &&
         let new_team =
           List.find
             (fun team -> same_team team current_team)
             new_board_with_discard_pile.teams
         in
         let new_player =
           List.find
             (fun player -> have_same_id_player player current_player)
             (get_players_from new_team)
         in
         let new_hand = get_hand_from new_player in
         List.length new_hand = List.length current_player_hand - 1
         && (not (List.mem (Safety EmergencyVehicle) new_hand))
         && peek_card_from_pile new_board_with_discard_pile.discard_pile
            == Safety EmergencyVehicle
         && List.length new_board_with_discard_pile.discard_pile
            = List.length
                discard_card_board_with_team_with_current_player_with_non_empty_hand
                  .discard_pile
              + 1))

let test_place_card_from_team_not_in_board =
  Alcotest.test_case "raise Team_not_found on place card from team not in board"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Team_not_found" Team_not_found (fun () ->
          ignore
            (place_card
               place_card_board_with_team_with_current_player_with_empty_hand
               place_card_team_not_in_board (Safety EmergencyVehicle)
               place_card_team_with_current_player_with_empty_hand)))

let test_place_card_to_team_not_in_board =
  Alcotest.test_case "raise Team_not_found on place card to team not in board"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Team_not_found" Team_not_found (fun () ->
          ignore
            (place_card
               place_card_board_with_team_with_current_player_with_empty_hand
               place_card_team_with_current_player_with_empty_hand
               (Safety EmergencyVehicle) place_card_team_not_in_board)))

let test_place_card_from_team_with_current_player_with_empty_hand =
  Alcotest.test_case
    "raise Empty_deck on place card from team with current player with empty \
     hand"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Empty_deck" Empty_deck (fun () ->
          ignore
            (place_card
               place_card_board_with_team_with_current_player_with_empty_hand
               place_card_team_with_current_player_with_empty_hand
               (Safety EmergencyVehicle)
               place_card_team_with_current_player_with_empty_hand)))

let test_place_card_from_team_with_current_player_without_card_within_hand =
  Alcotest.test_case
    "raise Card_not_found on place card from team with current player without \
     card within hand"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Card_not_found" Card_not_found (fun () ->
          ignore
            (place_card
               place_card_board_with_team_with_current_player_with_non_empty_hand
               place_card_team_with_current_player_with_non_empty_hand
               (Hazard OutOfGas)
               place_card_team_with_current_player_with_non_empty_hand)))

let test_place_attack_card_from_team_with_current_player_with_card_within_hand_to_same_team
    =
  Alcotest.test_case
    "raise Invalid_move on place attack card from team with current player \
     with card within hand to same team"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Invalid_move" Invalid_move (fun () ->
          ignore
            (place_card
               place_card_board_with_team_with_current_player_with_non_empty_hand
               place_card_team_with_current_player_with_non_empty_hand
               (Hazard Accident)
               place_card_team_with_current_player_with_non_empty_hand)))

let test_place_defend_card_from_team_with_current_player_with_card_within_hand_to_another_team
    =
  Alcotest.test_case
    "raise Invalid_move on place defend card from team with current player \
     with card within hand to another team"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Invalid_move" Invalid_move (fun () ->
          ignore
            (place_card place_card_board_with_teams
               place_card_team_with_current_player_with_non_empty_hand
               (Safety EmergencyVehicle)
               place_card_team_with_current_player_with_empty_hand)))

let test_place_distance_card_from_team_with_current_player_with_card_within_hand_to_another_team
    =
  Alcotest.test_case
    "raise Invalid_move on place distance card from team with current player \
     with card within hand to another team"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Invalid_move" Invalid_move (fun () ->
          ignore
            (place_card place_card_board_with_teams
               place_card_team_with_current_player_with_non_empty_hand
               (Distance D25)
               place_card_team_with_current_player_with_empty_hand)))

let test_place_attack_card_from_team_with_current_player_with_card_within_hand_to_another_team
    =
  Alcotest.test_case
    "place attack card from team with current player with card within hand to \
     another team"
    `Quick (fun () ->
      Alcotest.(check bool)
        "place attack card from team with current player with card within hand \
         to another team"
        true
        (let board = place_card_board_with_teams in
         let card = Hazard Accident in
         let team_from = place_card_team_with_current_player_with_non_empty_hand
         and team_to = place_card_team_with_current_player_with_empty_hand in
         let current_player_team_from = get_current_player_from team_from
         and current_player_team_to = get_current_player_from team_to in
         let new_board = place_card board team_from card team_to in
         compare_all_hands_except_two_players board new_board
           current_player_team_from current_player_team_to
         &&
         let new_team_from =
           List.find (fun team -> same_team team team_from) new_board.teams
         and new_team_to =
           List.find (fun team -> same_team team team_to) new_board.teams
         in
         let new_player_team_from =
           List.find
             (fun player -> have_same_id_player player current_player_team_from)
             (get_players_from new_team_from)
         in
         List.length (get_hand_from new_player_team_from)
         = List.length (get_hand_from current_player_team_from) - 1
         && (not (List.mem card (get_hand_from new_player_team_from)))
         && equal_card
              (peek_card_from_pile
                 (new_team_to |> get_public_informations_from
                |> get_drive_pile_from))
              card))

let test_place_defend_card_from_team_with_current_player_with_card_within_hand_to_same_team
    =
  Alcotest.test_case
    "place defend card from team with current player with card within hand to \
     same team"
    `Quick (fun () ->
      Alcotest.(check bool)
        "place defend card from team with current player with card within hand \
         to same team"
        true
        (let board = place_card_board_with_teams in
         let card = Safety EmergencyVehicle in
         let team = place_card_team_with_current_player_with_non_empty_hand in
         let current_player = get_current_player_from team in
         let new_board = place_card board team card team in
         compare_all_hands_except_player board new_board current_player
         &&
         let new_team = List.find (fun t -> same_team t team) new_board.teams in
         let new_player =
           List.find
             (fun player -> have_same_id_player player current_player)
             (get_players_from new_team)
         in
         let new_player_hand = get_hand_from new_player in
         List.length new_player_hand
         = List.length (get_hand_from current_player) - 1
         && (not (List.mem card new_player_hand))
         && List.mem card
              (new_team |> get_public_informations_from |> get_safety_area_from)))

let test_place_distance_card_from_team_with_current_player_with_card_within_hand_to_same_team
    =
  Alcotest.test_case
    "place distance card from team with current player with card within hand \
     to same team"
    `Quick (fun () ->
      Alcotest.(check bool)
        "place distance card from team with current player with card within \
         hand to same team"
        true
        (let board = place_card_board_with_teams in
         let card = Distance D25 in
         let team = place_card_team_with_current_player_with_non_empty_hand in
         let current_player = get_current_player_from team in
         let new_board = place_card board team card team in
         compare_all_hands_except_player board new_board current_player
         &&
         let new_team = List.find (fun t -> same_team t team) new_board.teams in
         let new_player =
           List.find
             (fun player -> have_same_id_player player current_player)
             (get_players_from new_team)
         in
         let new_player_hand = get_hand_from new_player in
         List.length new_player_hand
         = List.length (get_hand_from current_player) - 1
         && (not (List.mem card new_player_hand))
         && List.mem card
              (new_team |> get_public_informations_from
             |> get_distance_cards_from)))

let test_place_coup_fouree2 =
  Alcotest.test_case "raise Player_not_found" `Quick (fun () ->
      Alcotest.check_raises "Expected Player_not_found" Player_not_found
        (fun () ->
          ignore (place_coup_fouree board1 team1 player21 EmergencyVehicle)))

let test_place_coup_fouree3 =
  Alcotest.test_case "raise Team_not_found" `Quick (fun () ->
      Alcotest.check_raises "Expected Team_not_found" Team_not_found (fun () ->
          ignore (place_coup_fouree board1 team3 player31 EmergencyVehicle)))

let test_place_coup_fouree4 =
  Alcotest.test_case "raise Empty_deck" `Quick (fun () ->
      Alcotest.check_raises "Expected Empty_deck" Empty_deck (fun () ->
          ignore (place_coup_fouree board1 team2 player21 EmergencyVehicle)))

let test_place_coup_fouree5 =
  Alcotest.test_case "raise Card_not_found" `Quick (fun () ->
      Alcotest.check_raises "Expected Card_not_found" Card_not_found (fun () ->
          ignore (place_coup_fouree board1 team1 player11 FuelTruck)))

let test_place_coup_fouree6 =
  Alcotest.test_case "raise Unusable_card" `Quick (fun () ->
      Alcotest.check_raises "Expected Unusable_card" Unusable_card (fun () ->
          ignore (place_coup_fouree board1 team2 player22 FuelTruck)))

let test_is_draw_pile_empty_on_not_empty =
  let open QCheck in
  Test.make ~count:1000
    ~name:
      "Forall non-empty draw_pile, when board uses draw_pile, \
       (is_draw_pile_empty board) = false" (list arbitrary_card) (fun l ->
      assume (l <> []);
      let board = { board1 with draw_pile = l } in
      not (is_draw_pile_empty board))

let test_is_draw_pile_empty_on_empty =
  Alcotest.test_case
    "When the board's draw_pile is [], (is_empty_draw_pile board) = true" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same" true
        (let board = { board1 with draw_pile = [] } in
         is_draw_pile_empty board))

let test_is_discard_pile_empty_on_non_empty =
  let open QCheck in
  Test.make ~count:1000
    ~name:
      "Forall non-empty discard_pile, when board uses discard_pile, \
       (is_discard_pile_empty board) = false" (list arbitrary_card) (fun l ->
      assume (l <> []);
      let board = { board1 with discard_pile = l } in
      not (is_discard_pile_empty board))

let test_is_discard_pile_empty_on_empty =
  Alcotest.test_case
    "When the board's discard_pile is [], (is_empty_discard_pile board) = true"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same" true
        (let board = { board1 with discard_pile = [] } in
         is_discard_pile_empty board))

let test_attack_hazard_stop_to_schumacher_team =
  Alcotest.test_case
    "raise Unusable card for hazard Stop to team with safety card Emergency \
     Vehicle"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Unusable_card" Unusable_card (fun () ->
          ignore
            (place_card board_attack_schumacher attacker_team (Hazard Stop)
               team_schumacher)))

let test_attack_hazard_speedlimit_to_schumacher_team =
  Alcotest.test_case
    "raise Unusable card for hazard SpeedLimit to team with safety card \
     Emergency Vehicle"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Unusable_card" Unusable_card (fun () ->
          ignore
            (place_card board_attack_schumacher attacker_team
               (Hazard SpeedLimit) team_schumacher)))

let test_attack_hazard_accident_to_kubica_team =
  Alcotest.test_case
    "raise Unusable card for hazard Accident to team with safety card Driving \
     Ace"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Unusable_card" Unusable_card (fun () ->
          ignore
            (place_card board_attack_kubica attacker_team (Hazard Accident)
               team_gigakub)))

let test_attack_hazard_flattire_to_alonso_team =
  Alcotest.test_case
    "raise Unusable card for hazard Flattire to team with safety card Puncture \
     Proof"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Unusable_card" Unusable_card (fun () ->
          ignore
            (place_card board_attack_alonso attacker_team (Hazard FlatTire)
               team_alonso)))

let test_attack_hazard_outofgas_to_massa_team =
  Alcotest.test_case
    "raise Unusable card for hazard OutOfGas to team with safety card Fuel \
     truck"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Unusable_card" Unusable_card (fun () ->
          ignore
            (place_card board_attack_massa attacker_team (Hazard OutOfGas)
               team_massa)))

let () =
  Random.self_init ();
  let open Alcotest in
  run "Board_engine"
    [
      ("switch current team from", [ test_switch_current_team_from ]);
      ( "switch current player from current team",
        [ test_switch_current_player_of_current_team_from ] );
      ( "draw card from draw pile",
        [
          test_draw_card_from_draw_pile_for_team_not_in_game;
          test_draw_card_from_non_empty_draw_pile;
        ] );
      ( "draw card from discard pile",
        [
          test_draw_card_from_empty_discard_pile;
          test_draw_card_from_discard_pile_for_team_not_in_game;
          test_draw_card_from_non_empty_discard_pile;
        ] );
      ( "switch draw and discard pile",
        [
          test_switch_draw_and_discard_pile_from_empty_draw_pile;
          test_switch_draw_and_discard_pile_from_empty_draw_pile_and_heavy_discard_pile;
        ] );
      ( "discard card from team",
        [
          test_discard_card_from_team_not_in_board;
          test_discard_card_from_team_with_current_player_with_empty_hand;
          test_discard_card_from_team_with_current_player_without_card_within_hand;
          test_discard_card_from_team_with_current_player_with_card_within_hand;
        ] );
      ( "place card from team to team",
        [
          test_place_card_from_team_not_in_board;
          test_place_card_to_team_not_in_board;
          test_place_card_from_team_with_current_player_with_empty_hand;
          test_place_card_from_team_with_current_player_without_card_within_hand;
          test_place_attack_card_from_team_with_current_player_with_card_within_hand_to_same_team;
          test_place_defend_card_from_team_with_current_player_with_card_within_hand_to_another_team;
          test_place_distance_card_from_team_with_current_player_with_card_within_hand_to_another_team;
          test_place_attack_card_from_team_with_current_player_with_card_within_hand_to_another_team;
          test_place_defend_card_from_team_with_current_player_with_card_within_hand_to_same_team;
          test_place_distance_card_from_team_with_current_player_with_card_within_hand_to_same_team;
        ] );
      ( "place coup fouree from team",
        [
          test_place_coup_fouree2;
          test_place_coup_fouree3;
          test_place_coup_fouree4;
          test_place_coup_fouree5;
          test_place_coup_fouree6;
        ] );
      ( "test is_draw_pile_empty",
        [
          test_is_draw_pile_empty_on_empty;
          QCheck_alcotest.to_alcotest test_is_draw_pile_empty_on_not_empty;
        ] );
      ( "test is_discard_pile_empty",
        [
          test_is_discard_pile_empty_on_empty;
          QCheck_alcotest.to_alcotest test_is_discard_pile_empty_on_non_empty;
        ] );
      ( "test hazard cards on Schumacher team (safety emergency vehicle)",
        [
          test_attack_hazard_stop_to_schumacher_team;
          test_attack_hazard_speedlimit_to_schumacher_team;
        ] );
      ( "test hazard cards on Kubica team (safety driving ace)",
        [ test_attack_hazard_accident_to_kubica_team ] );
      ( "test hazard cards on Alonso team (safety puncture proof)",
        [ test_attack_hazard_flattire_to_alonso_team ] );
      ( "test hazard cards on Massa team (safety fuel truck)",
        [ test_attack_hazard_outofgas_to_massa_team ] );
    ]
