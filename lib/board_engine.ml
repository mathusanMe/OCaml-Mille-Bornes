open Cards_engine
open Teams_engine

type board = {
  draw_pile : pile_of_card;
  discard_pile : pile_of_card;
  teams : team list;
  current_team_index : int;
}

exception Current_team_index_out_of_bound

let get_current_team_from (b : board) =
  if b.current_team_index < 0 || b.current_team_index >= List.length b.teams
  then raise Current_team_index_out_of_bound
  else List.nth b.teams b.current_team_index

let switch_current_player_of_current_team_from (b : board) =
  let current_team = get_current_team_from b in
  let new_team = set_next_player_from current_team in
  { b with teams = replace_team_in b.teams new_team }

let switch_current_team_from (b : board) =
  {
    b with
    current_team_index = (b.current_team_index + 1) mod List.length b.teams;
  }

let update_hand_for_player_in_team (b : board) (t : team) (p : player)
    (new_hand : deck_of_card) =
  let player_struct = get_player_struct_from p in
  let new_player_struct = { player_struct with hand = new_hand } in
  let new_player = replace_player_struct_in p new_player_struct in
  let new_team = replace_player_in t new_player in
  replace_team_in b.teams new_team (* Returns `team list` *)

exception Draw_pile_too_small

let draw_initial_hand_to_team team draw_pile =
  let rec aux_draw_to_team team draw_pile acc =
    if acc >= 6 then (team, draw_pile)
    else
      let players = team.players in
      let new_draw_pile, new_players =
        List.fold_left
          (fun (pile, p_list) p ->
            let c, new_pile = draw_card_from_pile pile in
            let p_struct = get_player_struct_from p in
            let new_p_struct =
              { p_struct with hand = add_card_to_deck p_struct.hand c }
            in
            let new_p = replace_player_struct_in p new_p_struct in
            (new_pile, new_p :: p_list))
          (draw_pile, []) players
      in
      let new_team = { team with players = List.rev new_players } in
      aux_draw_to_team new_team new_draw_pile (acc + 1)
  in
  try aux_draw_to_team team draw_pile 0
  with Empty_pile -> raise Draw_pile_too_small

let draw_initial_hand_to_teams (b : board) =
  let rec aux_draw_initial_hand_to_teams team_list acc_team_list draw_pile =
    match team_list with
    | [] -> (List.rev acc_team_list, draw_pile)
    | hd :: tl ->
        let new_team, new_pile = draw_initial_hand_to_team hd draw_pile in
        aux_draw_initial_hand_to_teams tl (new_team :: acc_team_list) new_pile
  in
  let new_team_list, new_draw_pile =
    aux_draw_initial_hand_to_teams b.teams [] b.draw_pile
  in
  { b with draw_pile = new_draw_pile; teams = new_team_list }

let is_draw_pile_empty b = is_empty b.draw_pile
let is_discard_pile_empty b = is_empty b.discard_pile

exception No_more_card

let swap_draw_and_shuffled_discard_pile b =
  let new_draw_pile = b.discard_pile |> shuffle_pile in
  let new_discard_pile = b.draw_pile in
  if is_empty new_draw_pile && is_empty new_discard_pile then raise No_more_card
  else { b with draw_pile = new_draw_pile; discard_pile = new_discard_pile }

exception Team_not_found

let draw_card (b : board) (t : team) (from_discard_pile : bool) =
  let card, new_pile =
    if from_discard_pile then draw_card_from_pile b.discard_pile
    else
      let new_board =
        if is_draw_pile_empty b then swap_draw_and_shuffled_discard_pile b
        else b
      in
      draw_card_from_pile new_board.draw_pile
  in
  if not (List.mem t b.teams) then raise Team_not_found
  else
    let player = get_current_player_from t in
    let player_struct = get_player_struct_from player in
    let new_hand = add_card_to_deck player_struct.hand card in
    let new_teams = update_hand_for_player_in_team b t player new_hand in
    if from_discard_pile then
      { b with discard_pile = new_pile; teams = new_teams }
    else { b with draw_pile = new_pile; teams = new_teams }

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
      let new_discard_pile = add_card_to_pile b.discard_pile c in
      { b with discard_pile = new_discard_pile; teams = new_teams }

exception Player_not_found

let discard_card_from_player (b : board) (t : team) (p : player) (c : card) =
  if not (List.mem t b.teams) then raise Team_not_found
  else if not (List.mem p t.players) then raise Player_not_found
  else
    let player_struct = get_player_struct_from p in
    if player_struct.hand = [] then raise Empty_deck
    else if not (List.mem c player_struct.hand) then raise Card_not_found
    else
      let new_hand = remove_card_from_deck player_struct.hand c in
      let new_teams = update_hand_for_player_in_team b t p new_hand in
      let new_discard_pile = add_card_to_pile b.discard_pile c in
      { b with discard_pile = new_discard_pile; teams = new_teams }

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

let find_index f l =
  let rec aux_find_index i l =
    match l with
    | h :: t -> if f h then Some i else aux_find_index (i + 1) t
    | [] -> None
  in
  aux_find_index 0 l

let get_index_of_card_on_hand (c : card) (p : player) =
  let p_struct = get_player_struct_from p in
  let hand = p_struct.hand in
  let index = find_index (fun x -> equal_card x c) hand in
  match index with Some i -> i | None -> raise Card_not_found

let set_previous_current_team_from (b : board) (t : team) =
  let new_current_team_index =
    find_index
      (fun x ->
        x.shared_public_informations.id = t.shared_public_informations.id)
      b.teams
  in
  let previous_new_current_team_index =
    match new_current_team_index with
    | None -> raise Team_not_found
    | Some i -> i - 1
  in
  let previous_new_current_team_index =
    if previous_new_current_team_index = -1 then List.length b.teams - 1
    else previous_new_current_team_index
  in
  { b with current_team_index = previous_new_current_team_index }

let draw_card_with_hand (b : board) (hand : deck_of_card) =
  let card, new_draw_pile = draw_card_from_pile b.draw_pile in
  let new_hand = add_card_to_deck hand card in
  (new_draw_pile, new_hand)

let place_coup_fouree (b : board) (t_from : team) (player_from : player)
    (safety : safety_card) =
  let teams_found = List.mem t_from b.teams in
  if not teams_found then raise Team_not_found
  else
    let player_found = List.mem player_from t_from.players in
    if not player_found then raise Player_not_found
    else
      let player_from_struct = get_player_struct_from player_from in
      if player_from_struct.hand = [] then raise Empty_deck
      else if not (List.mem (Safety safety) player_from_struct.hand) then
        raise Card_not_found
      else if
        not (is_usable_card t_from.shared_public_informations (Safety safety))
      then raise Unusable_card
      else
        let new_board =
          if is_draw_pile_empty b then swap_draw_and_shuffled_discard_pile b
          else b
        in
        let new_team = use_coup_fouree t_from safety in
        let new_player = player_from in
        let new_player_struct = get_player_struct_from new_player in
        let new_hand =
          remove_card_from_deck new_player_struct.hand (Safety safety)
        in
        let new_draw_pile, new_hand = draw_card_with_hand new_board new_hand in
        let new_teams =
          update_hand_for_player_in_team new_board new_team new_player new_hand
        in
        set_previous_current_team_from
          { new_board with draw_pile = new_draw_pile; teams = new_teams }
          new_team
