open Mille_bornes.Board_engine
open Mille_bornes.Teams_engine
open Mille_bornes.Cards_engine
open Utils_board_engine

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

let test_draw_card_from_draw_pile_for_team_not_in_game =
  Alcotest.test_case
    "raise TeamNotFound on draw from draw pile for team not in game" `Quick
    (fun () ->
      Alcotest.check_raises "Expected Team_not_found" Team_not_found (fun () ->
          ignore (draw_card draw_card_board_with_draw_pile team_not_in_board)))

let test_draw_card_from_empty_draw_pile =
  Alcotest.test_case "raise EmptyPile on draw from empty draw pile" `Quick
    (fun () ->
      Alcotest.check_raises "Expected EmptyPile" Empty_pile (fun () ->
          ignore
            (draw_card draw_card_board_with_empty_draw_pile
               (get_current_team_from draw_card_board_with_empty_draw_pile))))

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
         let current_player_struct = get_player_struct_from current_player in
         let current_player_hand = current_player_struct.hand in
         let new_board_with_draw_pile =
           draw_card draw_card_board_with_draw_pile current_team
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
             (fun player -> same_player player current_player)
             new_team.players
         in
         let new_player_struct = get_player_struct_from new_player in
         let new_hand = new_player_struct.hand in
         List.mem card new_hand
         && List.length new_hand = List.length current_player_hand + 1
         && List.length draw_card_board_with_draw_pile.draw_pile
            = List.length new_board_with_draw_pile.draw_pile + 1))

let test_switch_draw_and_discard_pile_from_empty_draw_pile =
  Alcotest.test_case "switch empty draw pile and discard pile" `Quick (fun () ->
      Alcotest.(check bool)
        "test if the discard_pile of new board and draw pile of the tested \
         board are the same and the draw pile of the new board and the discard \
         pile of the tested board are the same on content"
        true
        (let new_board =
           swap_draw_and_shuffled_discard_pile board_with_empty_draw_pile
         in
         is_discard_pile_empty new_board
         && equal_pile_of_card
              (sort_card_list new_board.draw_pile)
              (sort_card_list board_with_empty_draw_pile.discard_pile)))

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
  Alcotest.test_case "raise TeamNotFound on discard card from team not in board"
    `Quick (fun () ->
      Alcotest.check_raises "Expected TeamNotFound" TeamNotFound (fun () ->
          ignore
            (discard_card
               discard_card_board_with_team_with_current_player_with_empty_hand
               discard_card_team_not_in_board (Safety EmergencyVehicle))))

let test_discard_card_from_team_with_current_player_with_empty_hand =
  Alcotest.test_case
    "raise EmptyDeck on discard card from team with current player with empty \
     hand"
    `Quick (fun () ->
      Alcotest.check_raises "Expected EmptyDeck" EmptyDeck (fun () ->
          ignore
            (discard_card
               discard_card_board_with_team_with_current_player_with_empty_hand
               discard_card_team_with_current_player_with_empty_hand
               (Safety EmergencyVehicle))))

let test_discard_card_from_team_with_current_player_without_card_within_hand =
  Alcotest.test_case
    "raise CardNotFound on discard card from team with current player without \
     card within hand"
    `Quick (fun () ->
      Alcotest.check_raises "Expected CardNotFound" CardNotFound (fun () ->
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
         let current_player_struct = get_player_struct_from current_player in
         let current_player_hand = current_player_struct.hand in
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
             (fun player -> same_player player current_player)
             new_team.players
         in
         let new_player_struct = get_player_struct_from new_player in
         let new_hand = new_player_struct.hand in
         List.length new_hand = List.length current_player_hand - 1
         && (not (List.mem (Safety EmergencyVehicle) new_hand))
         && peek_card_from_pile new_board_with_discard_pile.discard_pile
            == Safety EmergencyVehicle
         && List.length new_board_with_discard_pile.discard_pile
            = List.length
                discard_card_board_with_team_with_current_player_with_non_empty_hand
                  .discard_pile
              + 1))

let () =
  let open Alcotest in
  run "Board_engine"
    [
      ( "draw card from draw pile",
        [
          test_draw_card_from_empty_draw_pile;
          test_draw_card_from_draw_pile_for_team_not_in_game;
          test_draw_card_from_non_empty_draw_pile;
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
    ]
