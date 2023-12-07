open Teams_engine
open Cards_engine
open Board_engine

let random_boolean () = Random.bool ()

let random_bot_choose_card_to_play (p : player) (p_info : public_informations)
    (p_info_list : public_informations list) =
  let hand = get_hand_from p in
  if is_empty hand then raise Empty_deck
  else
    let card = List.nth hand (Random.int (List.length hand)) in
    let index_card = get_index_of_card_on_hand card p in
    match card with
    | Remedy _ | Safety _ | Distance _ ->
        if is_usable_card p_info card then
          Some (index_card, Some (get_id_from p_info))
        else Some (index_card, None)
    | Hazard _ ->
        let can_be_attacked =
          List.find_all
            (fun public_info -> is_usable_card public_info card)
            p_info_list
        in
        if List.length can_be_attacked = 0 then Some (index_card, None)
        else
          Some
            ( index_card,
              Some
                (get_id_from
                   (List.nth can_be_attacked
                      (Random.int (List.length can_be_attacked)))) )

let random_bot_want_to_peek_discard_pile _ _ _ _ = Some (random_boolean ())
let random_bot_want_to_play_coup_fourre _ _ _ _ = Some (random_boolean ())

let random_strategy =
  {
    name = "Random strategy";
    choose_card_to_play = random_bot_choose_card_to_play;
    want_to_peek_discard_pile = random_bot_want_to_peek_discard_pile;
    want_to_play_coup_fourre = random_bot_want_to_play_coup_fourre;
  }
