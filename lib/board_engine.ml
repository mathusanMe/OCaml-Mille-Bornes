open Cards_engine
open Teams_engine

type board = {
  draw_pile : pile_of_card;
  discard_pile : pile_of_card;
  teams : team list;
  current_team_index : int;
}

let get_current_team_from (b : board) = List.nth b.teams b.current_team_index

exception Team_not_found

let draw_card (b : board) (t : team) =
  let card, new_draw_pile = draw_card_from_pile b.draw_pile in
  if not (List.mem t b.teams) then raise Team_not_found
  else
    let player = get_current_player_from t in
    let player_struct = get_player_struct_from player in
    let new_hand = add_card_to_deck player_struct.hand card in
    let new_player_struct = { player_struct with hand = new_hand } in
    let new_player = replace_player_struct_in player new_player_struct in
    let new_team = replace_player_in t new_player in
    let new_teams = replace_team_in b.teams new_team in
    { b with draw_pile = new_draw_pile; teams = new_teams }

let is_draw_pile_empty b = is_empty b.draw_pile
let is_discard_pile_empty b = is_empty b.discard_pile

let swap_draw_and_shuffled_discard_pile b =
  let new_draw_pile = b.discard_pile |> shuffle_pile in
  let new_discard_pile = b.draw_pile in
  { b with draw_pile = new_draw_pile; discard_pile = new_discard_pile }
