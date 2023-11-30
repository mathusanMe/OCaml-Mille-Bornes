open Board_engine
open Cards_engine
open Teams_engine

exception Hazard_on_the_drive_pile_missing
exception Speed_limit_on_the_drive_pile
exception Hazard_not_speed_limit_on_the_speed_pile

let rec get_maximum_playable_distance_card (p : player)
    (p_info : public_informations) (distance_card_list : card list) =
  match distance_card_list with
  | [] -> None
  | d :: h ->
      if is_card_in_player_hand p d && is_usable_card p_info d then Some d
      else get_maximum_playable_distance_card p p_info h

let have_any_hazard_card_for_drive_pile_on_hand (p : player) =
  List.filter
    (fun x ->
      match x with Hazard SpeedLimit -> false | Hazard _ -> true | _ -> false)
    (get_player_struct_from p).hand

let have_any_speed_limit_card_on_hand (p : player) =
  List.filter
    (fun x -> match x with Hazard SpeedLimit -> true | _ -> false)
    (get_player_struct_from p).hand

let get_unprotected_opponent_with_highest_mileage (p : player)
    (p_info_list : public_informations list) (hazard_card_list : card list) =
  let sorted_by_score_p_info_list =
    List.sort (fun x y -> compare x.score y.score) p_info_list
  in
  let rec loop_p_info_list p_info_list =
    match p_info_list with
    | [] -> None
    | p_info :: t1 ->
        let rec loop_hazard_card_on_hand_list hazard_card_on_hand_list =
          match hazard_card_on_hand_list with
          | [] -> loop_p_info_list t1
          | hazard_card :: t2 ->
              if is_usable_card p_info hazard_card then
                Some (get_index_of_card_on_hand hazard_card p, Some p_info.id)
              else loop_hazard_card_on_hand_list t2
        in
        loop_hazard_card_on_hand_list hazard_card_list
  in
  loop_p_info_list sorted_by_score_p_info_list

let sort_hand_by_less_important (hand : card list) =
  let compare a b =
    match (a, b) with
    | Safety _, _ -> -1
    | _, Safety _ -> 1
    | Remedy _, _ -> -1
    | _, Remedy _ -> 1
    | Distance D25, _ -> 1
    | _, Distance D25 -> -1
    | Distance D50, _ -> 1
    | _, Distance D50 -> -1
    | Distance _, _ -> -1
    | _, Distance _ -> 1
    | _, _ -> 0
  in
  List.sort compare hand

let get_weakest_card (p : player) =
  let p_struct = get_player_struct_from p in
  let hand = p_struct.hand in
  let sorted_hand_by_less_important = sort_hand_by_less_important hand in
  match sorted_hand_by_less_important with
  | [] -> raise Empty_deck
  | h :: _ -> h

let branch (p : player) (p_info : public_informations)
    (p_info_list : public_informations list) =
  let hazard_card_list_for_drive_pile_on_hand =
    have_any_hazard_card_for_drive_pile_on_hand p
  in
  let unprotected_opponent_with_highest_mileage =
    get_unprotected_opponent_with_highest_mileage p p_info_list
      hazard_card_list_for_drive_pile_on_hand
  in
  match unprotected_opponent_with_highest_mileage with
  | None -> (
      let speed_limit_card_list_on_hand = have_any_speed_limit_card_on_hand p in
      let unprotected_opponent_with_highest_mileage =
        get_unprotected_opponent_with_highest_mileage p p_info_list
          speed_limit_card_list_on_hand
      in

      match unprotected_opponent_with_highest_mileage with
      | None -> (
          let safety_card_list_on_hand =
            List.filter
              (fun x -> match x with Safety _ -> true | _ -> false)
              (get_player_struct_from p).hand
          in
          match safety_card_list_on_hand with
          | [] -> Some (get_index_of_card_on_hand (get_weakest_card p) p, None)
          | h :: _ -> Some (get_index_of_card_on_hand h p, Some p_info.id))
      | not_none -> not_none)
  | not_none -> not_none

let sad_bot_choose_card_to_play (p : player) (p_info : public_informations)
    (p_info_list : public_informations list) =
  if is_attacked_by_hazard_on_drive_pile p_info then
    let hazard_card = peek_card_from_pile p_info.drive_pile in
    let corresponding_safety_card =
      match hazard_card with
      | Hazard h -> Safety (get_safety_corresponding_to_the_hazard h)
      | _ -> raise Hazard_on_the_drive_pile_missing
    in
    if
      is_card_in_player_hand p corresponding_safety_card
      && is_usable_card p_info corresponding_safety_card
    then
      Some
        (get_index_of_card_on_hand corresponding_safety_card p, Some p_info.id)
    else
      let corresponding_remedy_card =
        match hazard_card with
        | Hazard h -> Remedy (get_remedy_corresponding_to_the_hazard h)
        | _ -> raise Hazard_on_the_drive_pile_missing
      in
      if
        is_card_in_player_hand p corresponding_remedy_card
        && is_usable_card p_info corresponding_remedy_card
      then
        Some
          (get_index_of_card_on_hand corresponding_remedy_card p, Some p_info.id)
      else branch p p_info p_info_list
  else if is_attacked_by_speed_limit p_info then
    let end_of_speed_limit_card = Remedy EndOfSpeedLimit in
    if is_card_in_player_hand p end_of_speed_limit_card then
      Some (get_index_of_card_on_hand end_of_speed_limit_card p, Some p_info.id)
    else
      let distance_card =
        [ Distance D50; Distance D25 ]
        |> get_maximum_playable_distance_card p p_info
      in
      match distance_card with
      | Some c -> Some (get_index_of_card_on_hand c p, Some p_info.id)
      | None -> branch p p_info p_info_list
  else
    let distance_card =
      [ Distance D200; Distance D100; Distance D75; Distance D50; Distance D25 ]
      |> get_maximum_playable_distance_card p p_info
    in
    match distance_card with
    | Some c -> Some (get_index_of_card_on_hand c p, Some p_info.id)
    | None -> branch p p_info p_info_list

let sad_bot_want_to_peek_discard_pile (p : player) (c : card)
    (p_info : public_informations) (p_info_list : public_informations list) =
  let speed_limit_pile = p_info.speed_limit_pile in
  let drive_pile = p_info.drive_pile in
  match c with
  | Safety _ -> Some true
  | Remedy EndOfSpeedLimit ->
      if not (is_empty speed_limit_pile) then
        let top_card_speed_limit_pile = peek_card_from_pile speed_limit_pile in
        match top_card_speed_limit_pile with
        | Hazard SpeedLimit -> Some true
        | _ -> Some false
      else Some false
  | Remedy r ->
      if not (is_empty drive_pile) then
        let top_card_drive_pile = peek_card_from_pile drive_pile in
        match top_card_drive_pile with
        | Hazard h ->
            Some
              (equal_remedy_card (get_remedy_corresponding_to_the_hazard h) r)
        | _ -> Some false
      else Some false
  | Distance d ->
      let bool =
        [ Distance d ]
        |> get_maximum_playable_distance_card p p_info
        |> Option.is_some
      in
      Some bool
  | Hazard h ->
      let bool =
        [ Hazard h ]
        |> get_unprotected_opponent_with_highest_mileage p p_info_list
        |> Option.is_some
      in
      Some bool

let sad_bot_want_to_play_coup_fourre _ _ _ _ = Some true

let sad_strategy =
  {
    name = "Sad strategy";
    choose_card_to_play = sad_bot_choose_card_to_play;
    want_to_peek_discard_pile = sad_bot_want_to_peek_discard_pile;
    want_to_play_coup_fourre = sad_bot_want_to_play_coup_fourre;
  }
