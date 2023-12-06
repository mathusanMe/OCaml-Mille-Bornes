open Cards_engine

type player_struct = { id : int; name : string; hand : deck_of_card }

type public_informations = {
  id : int;
  speed_limit_pile : pile_of_card;
  drive_pile : pile_of_card;
  distance_cards : deck_of_card;
  safety_area : deck_of_card;
  coup_fouree_cards : deck_of_card;
  score : int;
}

type player = player_struct * strategy

and strategy = {
  name : string;
  choose_card_to_play :
    player ->
    public_informations ->
    public_informations list ->
    (int * int option) option;
  want_to_peek_discard_pile :
    player ->
    card ->
    public_informations ->
    public_informations list ->
    bool option;
  want_to_play_coup_fourre :
    player ->
    hazard_card ->
    public_informations ->
    public_informations list ->
    bool option;
}

type team = {
  players : player list;
  shared_public_informations : public_informations;
  current_player_index : int;
}

let init_player_struct (entered_name : string) (id : int) =
  { id; name = entered_name; hand = [] }

let init_player (entered_name : string) (strat : strategy) (id : int) =
  (init_player_struct entered_name id, strat)

let init_public_informations (i : int) =
  {
    id = i;
    speed_limit_pile = [];
    drive_pile = [];
    distance_cards = [];
    safety_area = [];
    coup_fouree_cards = [];
    score = 0;
  }

let init_team_with_one_player (name : string) (strat : strategy) (id : int) =
  {
    players = [ init_player name strat 0 ];
    shared_public_informations = init_public_informations id;
    current_player_index = 0;
  }

exception Invalid_names

let init_team_with_two_players (name1 : string) (strat1 : strategy)
    (name2 : string) (strat2 : strategy) (id : int) =
  if name1 = name2 then raise Invalid_names
  else
    {
      players = [ init_player name1 strat1 0; init_player name2 strat2 1 ];
      shared_public_informations = init_public_informations id;
      current_player_index = 0;
    }

exception Current_player_index_out_of_bound

let get_current_player_from (t : team) =
  if
    t.current_player_index < 0
    || t.current_player_index >= List.length t.players
  then raise Current_player_index_out_of_bound
  else List.nth t.players t.current_player_index

let set_hand_from (p : player) (d : deck_of_card) =
  match p with p_struct, p_strat -> ({ p_struct with hand = d }, p_strat)

let get_hand_from (p : player) = match p with p_struct, _ -> p_struct.hand
let get_name_from (p : player) = match p with p_struct, _ -> p_struct.name
let get_strat_from (p : player) = match p with _, p_strat -> p_strat
let get_player_struct_from (p : player) = match p with p_struct, _ -> p_struct
let get_names_from (t : team) = List.map (fun p -> get_name_from p) t.players

let have_same_contents_team t1 t2 =
  List.for_all2
    (fun p1 p2 ->
      get_player_struct_from p1 = get_player_struct_from p2
      && (get_strat_from p1).name = (get_strat_from p2).name)
    t1.players t2.players

let set_next_player_from (t : team) =
  if List.length t.players = 2 then
    {
      players = t.players;
      shared_public_informations = t.shared_public_informations;
      current_player_index = 1 - t.current_player_index;
    }
  else t

let have_same_id_player (p1 : player) (p2 : player) =
  match (p1, p2) with
  | (p1_struct, _), (p2_struct, _) -> p1_struct.id = p2_struct.id

let does_player_have_this_name_in_team_list name team_list =
  List.exists
    (fun t -> List.exists (fun p -> get_name_from p = name) t.players)
    team_list

let same_team (t1 : team) (t2 : team) =
  List.for_all2 have_same_id_player t1.players t2.players
  && t1.shared_public_informations.id = t2.shared_public_informations.id

let replace_player_in (t : team) (p : player) =
  {
    t with
    players =
      List.map (fun x -> if have_same_id_player x p then p else x) t.players;
  }

let replace_team_in (teams : team list) (t : team) =
  List.map (fun x -> if same_team x t then t else x) teams

let is_card_in_player_hand (p : player) (c : card) =
  let hand = get_hand_from p in
  List.mem c hand

let pp_player with_hand fmt (p : player) =
  let pp_hand b hand = if b then pp_deck_of_card "hand" fmt hand in
  match p with
  | p_struct, p_strat ->
      Format.fprintf fmt "%s (player with strategy %s)@ " p_struct.name
        p_strat.name;
      pp_hand with_hand p_struct.hand

let pp_player_list_with_all_hands fmt players =
  let pp_iter fmt =
    List.iter (fun p -> Format.fprintf fmt "%a" (pp_player true) p)
  in
  Format.fprintf fmt "Name(s) with deck :@ %a" pp_iter players

let pp_player_list_with_no_hand fmt players =
  let pp_iter fmt =
    List.iter (fun p -> Format.fprintf fmt "%a" (pp_player false) p)
  in
  Format.fprintf fmt "Name(s) :@ %a" pp_iter players

let pp_player_list_with_one_player_hand player fmt players =
  let pp_iter fmt =
    List.iter (fun p ->
        Format.fprintf fmt "%a" (pp_player (have_same_id_player p player)) p)
  in
  Format.fprintf fmt "Name(s) with deck of %s :@ %a" (get_name_from player)
    pp_iter players

let pp_public_informations with_entirely_pi fmt dz =
  Format.fprintf fmt "Driving Zone : @ %a@;%a@;%a%a%a"
    (if with_entirely_pi then pp_pile_of_card "Speed limit pile"
     else pp_top_pile_of_card "Top of speed limit pile")
    dz.speed_limit_pile
    (if with_entirely_pi then pp_pile_of_card "Drive pile"
     else pp_top_pile_of_card "Top of drive pile")
    dz.drive_pile
    (pp_deck_of_card "Distance cards")
    dz.distance_cards
    (pp_deck_of_card "Safety cards")
    dz.safety_area
    (pp_deck_of_card "Coup fourree cards")
    dz.coup_fouree_cards

let pp_team with_hand with_entirely_pi fmt team =
  let pp_player_list =
    if with_hand then pp_player_list_with_all_hands
    else pp_player_list_with_no_hand
  in
  Format.fprintf fmt "@[<v>%a@]Score : %d@ @[<v>%a@]@;" pp_player_list
    team.players team.shared_public_informations.score
    (pp_public_informations with_entirely_pi)
    team.shared_public_informations

let pp_team_with_hand_of player fmt team =
  Format.fprintf fmt "@[<v>%a@]Score : %d@ @[<v>%a@]@;"
    (pp_player_list_with_one_player_hand player)
    team.players team.shared_public_informations.score
    (pp_public_informations false)
    team.shared_public_informations

let pp_public_informations_list fmt pinfo_list =
  pinfo_list
  |> ((fun fmt ->
        List.iteri (fun i e ->
            Format.fprintf fmt "%d. @[<v>%a@]@;" i
              (pp_public_informations false)
              e))
     |> Format.fprintf fmt "@[<v>%a@]")

let pp_names_of_team_list fmt teams =
  let pp_print_name_team fmt t =
    List.iter (fun p -> Format.fprintf fmt "%s;" (get_name_from p)) t.players
  in
  let pp_print_team_with_id fmt t i =
    Format.fprintf fmt "Team %d : %a@;" i pp_print_name_team t
  in
  let pp_print_teams_with_id fmt teams =
    List.iteri (fun i t -> pp_print_team_with_id fmt t i) teams
  in
  Format.fprintf fmt "@[<v>%a@]@;" pp_print_teams_with_id teams

let has_already_used_safety_card (p_info : public_informations)
    (c : safety_card) =
  let f = List.exists (fun x -> x = Safety c) in
  f p_info.safety_area || f p_info.coup_fouree_cards

let has_safety_to_counter_hazard_on_public_informations
    (p_info : public_informations) (c : hazard_card) =
  let necessary_safety = get_safety_corresponding_to_the_hazard c in
  has_already_used_safety_card p_info necessary_safety

let has_safety_to_counter_hazard_on_his_hand (p : player) (c : hazard_card) =
  let necessary_safety = get_safety_corresponding_to_the_hazard c in
  let p_hand = match p with p_struct, _ -> p_struct.hand in
  List.exists (fun x -> x = Safety necessary_safety) p_hand

let is_attacked_by_hazard_on_drive_pile (p_info : public_informations) =
  (not (is_empty p_info.drive_pile))
  &&
  match List.hd p_info.drive_pile with
  | Hazard hazard ->
      not (has_safety_to_counter_hazard_on_public_informations p_info hazard)
  | _ -> false

let is_attacked_by_speed_limit (p_info : public_informations) =
  (not (is_empty p_info.speed_limit_pile))
  && List.hd p_info.speed_limit_pile = Hazard SpeedLimit
  && not (has_safety_to_counter_hazard_on_public_informations p_info SpeedLimit)

let add_card_to_speed_limit_pile (t : team) (c : card) =
  {
    t with
    shared_public_informations =
      {
        t.shared_public_informations with
        speed_limit_pile =
          add_card_to_pile t.shared_public_informations.speed_limit_pile c;
      };
  }

let add_card_to_drive_pile (t : team) (c : card) =
  {
    t with
    shared_public_informations =
      {
        t.shared_public_informations with
        drive_pile = add_card_to_pile t.shared_public_informations.drive_pile c;
      };
  }

let is_usable_hazard_card (p_info : public_informations) (c : hazard_card) =
  match c with
  | SpeedLimit ->
      (not
         (has_safety_to_counter_hazard_on_public_informations p_info SpeedLimit))
      && not (is_attacked_by_speed_limit p_info)
  | hazard ->
      if
        is_empty p_info.drive_pile
        && not (has_already_used_safety_card p_info EmergencyVehicle)
      then false
      else
        (not
           (has_safety_to_counter_hazard_on_public_informations p_info hazard))
        && not (is_attacked_by_hazard_on_drive_pile p_info)

let use_hazard_card (t : team) = function
  | SpeedLimit -> add_card_to_speed_limit_pile t (Hazard SpeedLimit)
  | hazard -> add_card_to_drive_pile t (Hazard hazard)

let is_usable_distance_card (p_info : public_informations) (c : distance_card) =
  if
    is_empty p_info.drive_pile
    && not (has_already_used_safety_card p_info EmergencyVehicle)
    || is_attacked_by_hazard_on_drive_pile p_info
  then false
  else
    match c with
    | D200 ->
        if not (is_attacked_by_speed_limit p_info) then
          List.fold_left
            (fun acc c -> if c = Distance D200 then acc + 1 else acc)
            0 p_info.distance_cards
          < 2
        else false
    | D25 | D50 -> true
    | _ -> not (is_attacked_by_speed_limit p_info)

let use_distance_card (t : team) (c : distance_card) =
  let value =
    match c with D25 -> 25 | D50 -> 50 | D75 -> 75 | D100 -> 100 | D200 -> 200
  in
  {
    t with
    shared_public_informations =
      {
        t.shared_public_informations with
        distance_cards =
          add_card_to_deck t.shared_public_informations.distance_cards
            (Distance c);
        score = t.shared_public_informations.score + value;
      };
  }

let is_usable_safety_card (p_info : public_informations) = function
  | safety_effect -> not (has_already_used_safety_card p_info safety_effect)

let add_card_to_safety_area (t : team) (s : safety_card) =
  {
    t with
    shared_public_informations =
      {
        t.shared_public_informations with
        safety_area =
          add_card_to_deck t.shared_public_informations.safety_area (Safety s);
      };
  }

let use_safety_card (t : team) = function
  | EmergencyVehicle -> add_card_to_safety_area t EmergencyVehicle
  | safety -> add_card_to_safety_area t safety

let add_card_to_coup_fouree (t : team) (s : safety_card) =
  {
    t with
    shared_public_informations =
      {
        t.shared_public_informations with
        coup_fouree_cards =
          add_card_to_deck t.shared_public_informations.coup_fouree_cards
            (Safety s);
      };
  }

let use_coup_fouree (t : team) = function
  | EmergencyVehicle ->
      add_card_to_coup_fouree
        {
          t with
          shared_public_informations =
            {
              t.shared_public_informations with
              score = t.shared_public_informations.score + 200;
            };
        }
        EmergencyVehicle
  | safety ->
      add_card_to_coup_fouree
        {
          t with
          shared_public_informations =
            {
              t.shared_public_informations with
              score = t.shared_public_informations.score + 200;
            };
        }
        safety

let is_usable_remedy_card (p_info : public_informations) = function
  | EndOfSpeedLimit ->
      is_attacked_by_speed_limit p_info
      && not (has_already_used_safety_card p_info EmergencyVehicle)
  | Drive ->
      (not (has_already_used_safety_card p_info EmergencyVehicle))
      && (is_empty p_info.drive_pile
         || is_attacked_by_hazard_on_drive_pile p_info
            && peek_card_from_pile p_info.drive_pile = Hazard Stop)
  | remedy ->
      is_attacked_by_hazard_on_drive_pile p_info
      &&
      let hazard = get_hazard_corresponding_to_the_remedy remedy in
      peek_card_from_pile p_info.drive_pile = Hazard hazard
      && not (has_safety_to_counter_hazard_on_public_informations p_info hazard)

let use_remedy_card (t : team) = function
  | EndOfSpeedLimit -> add_card_to_speed_limit_pile t (Remedy EndOfSpeedLimit)
  | Drive -> add_card_to_drive_pile t (Remedy Drive)
  | remedy -> add_card_to_drive_pile t (Remedy remedy)

let is_usable_card (p_info : public_informations) (c : card) =
  match c with
  | Hazard hazard -> is_usable_hazard_card p_info hazard
  | Remedy remedy -> is_usable_remedy_card p_info remedy
  | Safety safety -> is_usable_safety_card p_info safety
  | Distance distance_card -> is_usable_distance_card p_info distance_card

let use_card (t : team) (c : card) =
  match c with
  | Hazard hazard -> use_hazard_card t hazard
  | Remedy remedy -> use_remedy_card t remedy
  | Safety safety -> use_safety_card t safety
  | Distance distance_card -> use_distance_card t distance_card

exception Index_of_hand_out_of_bound

let nth_hand_player (p : player) (i : int) =
  match p with
  | p_struct, _ ->
      if i < 0 || i > List.length p_struct.hand then
        raise Index_of_hand_out_of_bound
      else List.nth p_struct.hand i
