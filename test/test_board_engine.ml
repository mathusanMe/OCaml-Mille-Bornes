open Mille_bornes.Board_engine
open Mille_bornes.Teams_engine
open Mille_bornes.Cards_engine
open Utils_board_engine

let flatten_teams (teams : team list) =
  teams |> List.map (fun team -> team.players) |> List.flatten

let test_draw_card_from_non_empty_draw_pile =
  Alcotest.test_case "draw card from non empty draw pile" `Quick (fun () ->
      Alcotest.(check bool)
        "draw and add card to current team's current player's hand" true
        (let draw_pile = board_with_draw_pile.draw_pile in
         let card = peek_card_from_draw_pile draw_pile in
         let current_team = get_current_team_from board_with_draw_pile in
         let current_player = get_current_player_from current_team in
         let current_player_struct = get_player_struct_from current_player in
         let current_player_hand = current_player_struct.hand in
         let new_board_with_draw_pile =
           draw_card board_with_draw_pile current_team
         in
         List.map2
           (fun player new_player -> (player, new_player))
           (flatten_teams board_with_draw_pile.teams)
           (flatten_teams new_board_with_draw_pile.teams)
         |> List.filter (fun (player, new_player) ->
                same_player player new_player
                && not (same_player player current_player))
         |> List.map (fun (player, new_player) ->
                ( get_player_struct_from player,
                  get_player_struct_from new_player ))
         |> List.for_all (fun (player, new_player) ->
                equal_deck_of_card player.hand new_player.hand)
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
         && List.length board_with_draw_pile.draw_pile
            = List.length new_board_with_draw_pile.draw_pile + 1))

let test_draw_card_from_empty_draw_pile =
  Alcotest.test_case "raise EmptyPile on draw from empty draw pile" `Quick
    (fun () ->
      Alcotest.check_raises "Expected EmptyPile" EmptyPile (fun () ->
          ignore
            (draw_card board_with_empty_draw_pile
               (get_current_team_from board_with_empty_draw_pile))))

let () =
  let open Alcotest in
  run "Board_engine"
    [
      ( "draw card from draw pile",
        [
          test_draw_card_from_non_empty_draw_pile;
          test_draw_card_from_empty_draw_pile;
        ] );
    ]
