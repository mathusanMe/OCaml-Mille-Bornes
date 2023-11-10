open Cards_engine
open Teams_engine

type board = {
  draw_pile : pile_of_card;
  discard_pile : pile_of_card;
  teams : team list;
  current_team_index : int;
}

let get_current_team_from (b : board) = List.nth b.teams b.current_team_index

let update_hand_for_player_in_team (b : board) (t : team) (p : player)
    (new_hand : deck_of_card) =
  let player_struct = get_player_struct_from p in
  let new_player_struct = { player_struct with hand = new_hand } in
  let new_player = replace_player_struct_in p new_player_struct in
  let new_team = replace_player_in t new_player in
  replace_team_in b.teams new_team (* Returns `team list` *)

exception Team_not_found

let draw_card (b : board) (t : team) =
  let card, new_draw_pile = draw_card_from_pile b.draw_pile in
  if not (List.mem t b.teams) then raise Team_not_found
  else
    let player = get_current_player_from t in
    let player_struct = get_player_struct_from player in
    let new_hand = add_card_to_deck player_struct.hand card in
    let new_teams = update_hand_for_player_in_team b t player new_hand in
    { b with draw_pile = new_draw_pile; teams = new_teams }

let is_draw_pile_empty b = is_empty b.draw_pile
let is_discard_pile_empty b = is_empty b.discard_pile

let swap_draw_and_shuffled_discard_pile b =
  let new_draw_pile = b.discard_pile |> shuffle_pile in
  let new_discard_pile = b.draw_pile in
  { b with draw_pile = new_draw_pile; discard_pile = new_discard_pile }

exception Card_not_found

let discard_card (b : board) (t : team) (c : card) =
  if not (List.mem t b.teams) then raise Team_not_found
  else
    let player = get_current_player_from t in
    let player_struct = get_player_struct_from player in
    if player_struct.hand = [] then raise Empty_deck
    else if not (List.mem c player_struct.hand) then raise Card_not_found
    else
      let new_hand = remove_card_from_deck player_struct.hand c in
      let new_teams = update_hand_for_player_in_team b t player new_hand in
      let discard_pile = add_card_to_pile b.discard_pile c in
      { b with discard_pile; teams = new_teams }

exception Invalid_move
exception Unusable_card

let place_card (b : board) (t_from : team) (c : card) (t_to : team) =
  let teams_found = List.mem t_from b.teams && List.mem t_to b.teams in
  if not teams_found then raise Team_not_found
  else
    let player_from = get_current_player_from t_from in
    let player_from_struct = get_player_struct_from player_from in
    if player_from_struct.hand = [] then raise Empty_deck
    else if not (List.mem c player_from_struct.hand) then raise Card_not_found
    else
      match c with
      | (Safety _ | Remedy _ | Distance _) when not (same_team t_from t_to) ->
          raise Invalid_move
      | Safety _ | Remedy _ | Distance _ ->
          if not (is_usable_card t_to.shared_public_informations c) then
            raise Unusable_card
          else
            let new_team = use_card t_to c in
            let new_current_player = get_current_player_from new_team in
            let new_current_player_struct =
              get_player_struct_from new_current_player
            in
            let new_hand =
              remove_card_from_deck new_current_player_struct.hand c
            in
            let new_teams =
              update_hand_for_player_in_team b new_team new_current_player
                new_hand
            in
            { b with teams = new_teams }
      | Hazard _ when same_team t_from t_to -> raise Invalid_move
      | Hazard _ ->
          if not (is_usable_card t_to.shared_public_informations c) then
            raise Unusable_card
          else
            let new_hand = remove_card_from_deck player_from_struct.hand c in
            let new_teams_from =
              update_hand_for_player_in_team b t_from player_from new_hand
            in
            let new_team_to = use_card t_to c in
            let new_teams_to = replace_team_in new_teams_from new_team_to in
            { b with teams = new_teams_to }
