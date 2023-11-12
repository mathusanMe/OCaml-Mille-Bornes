open Cards_engine

type player_struct = { name : string; hand : deck_of_card }

type public_informations = {
  id : int;
  speed_limit_pile : pile_of_card;
  drive_pile : pile_of_card;
  distance_cards : deck_of_card;
  safety_area : deck_of_card;
  coup_fouree_cards : deck_of_card;
  score : int;
  can_drive : bool;
}

type player = Computer of (player_struct * strategy) | Human of player_struct

and strategy = {
  name : string;
  choose_card_to_play :
    player ->
    public_informations ->
    public_informations list ->
    int * int option;
  want_to_peek_discard_pile :
    player -> card -> public_informations -> public_informations list -> bool;
  want_to_play_coup_fourre :
    player ->
    hazard_card ->
    public_informations ->
    public_informations list ->
    bool;
}

type team = {
  players : player list;
  shared_public_informations : public_informations;
  current_player_index : int;
}

let init_player_struct (entered_name : string) =
  { name = entered_name; hand = [] }

let init_human (entered_name : string) = Human (init_player_struct entered_name)

let init_computer (name : string) (strat : strategy) =
  Computer (init_player_struct name, strat)

let init_public_informations (i : int) =
  {
    id = i;
    speed_limit_pile = [];
    drive_pile = [];
    distance_cards = [];
    safety_area = [];
    coup_fouree_cards = [];
    score = 0;
    can_drive = false;
  }

let init_team_with_one_player (p : player) (id : int) =
  {
    players = [ p ];
    shared_public_informations = init_public_informations id;
    current_player_index = 0;
  }

let init_team_with_one_human (name : string) (id : int) =
  init_team_with_one_player (init_human name) id

let init_team_with_one_computer (name : string) (strat : strategy) (id : int) =
  init_team_with_one_player (init_computer name strat) id

let init_team_with_two_players (player1 : player) (player2 : player) (id : int)
    =
  {
    players = [ player1; player2 ];
    shared_public_informations = init_public_informations id;
    current_player_index = 0;
  }

let init_team_with_two_human (name1 : string) (name2 : string) (id : int) =
  let player1 = init_human name1 in
  let player2 = init_human name2 in
  init_team_with_two_players player1 player2 id

let init_team_with_one_human_and_one_computer (name1 : string)
    (is_computer1 : bool) (name2 : string) (is_computer2 : bool)
    (strat : strategy) (id : int) =
  let player1 =
    if is_computer1 then init_computer name1 strat else init_human name1
  in
  let player2 =
    if is_computer2 then init_computer name2 strat else init_human name2
  in
  init_team_with_two_players player1 player2 id

let init_team_with_two_computer (name1 : string) (strat1 : strategy)
    (name2 : string) (strat2 : strategy) (id : int) =
  let player1 = init_computer name1 strat1 in
  let player2 = init_computer name2 strat2 in
  init_team_with_two_players player1 player2 id

let get_current_player_from (t : team) =
  List.nth t.players t.current_player_index

let get_player_struct_from (p : player) =
  match p with Human p_struct -> p_struct | Computer (p_struct, _) -> p_struct

let set_next_player_from (t : team) =
  if List.length t.players = 2 then
    {
      players = t.players;
      shared_public_informations = t.shared_public_informations;
      current_player_index = 1 - t.current_player_index;
    }
  else t

let same_player (p1 : player) (p2 : player) =
  let e1 = get_player_struct_from p1 in
  let e2 = get_player_struct_from p2 in
  e1.name = e2.name

let does_player_have_this_name_in_team_list name team_list =
  List.exists
    (fun t ->
      List.exists
        (fun p ->
          let p_struct = get_player_struct_from p in
          p_struct.name = name)
        t.players)
    team_list

let same_team (t1 : team) (t2 : team) =
  List.for_all2 same_player t1.players t2.players
  && t1.shared_public_informations.id = t2.shared_public_informations.id

let replace_player_struct_in (p : player) (p_struct : player_struct) =
  match p with
  | Computer (_, strat) -> Computer (p_struct, strat)
  | Human _ -> Human p_struct

let replace_player_in (t : team) (p : player) =
  {
    t with
    players = List.map (fun x -> if same_player x p then p else x) t.players;
  }

let replace_team_in (teams : team list) (t : team) =
  List.map (fun x -> if same_team x t then t else x) teams

let pp_player with_hand fmt p =
  let pp_hand b hand = if b then pp_deck_of_card "hand" fmt hand in
  match p with
  | Computer (p_struct, p_strat) ->
      Format.fprintf fmt "%s (computer with strategy %s)@ " p_struct.name
        p_strat.name;
      pp_hand with_hand p_struct.hand
  | Human p_struct ->
      Format.fprintf fmt "%s@ " p_struct.name;
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
        Format.fprintf fmt "%a" (pp_player (same_player p player)) p)
  in
  Format.fprintf fmt "Name(s) with deck of %s :@ %a"
    (get_player_struct_from player).name pp_iter players

let pp_public_informations fmt dz =
  Format.fprintf fmt "Driving Zone : @ %a@;%a@;%a%a%a"
    (pp_top_pile_of_card "Top of speed limit pile")
    dz.speed_limit_pile
    (pp_top_pile_of_card "Top of drive pile")
    dz.drive_pile
    (pp_deck_of_card "Distance cards")
    dz.distance_cards
    (pp_deck_of_card "Safety cards")
    dz.safety_area
    (pp_deck_of_card "Coup fourree cards")
    dz.coup_fouree_cards

let pp_team with_hand fmt team =
  let pp_player_list =
    if with_hand then pp_player_list_with_all_hands
    else pp_player_list_with_no_hand
  in
  Format.fprintf fmt "@[<v>%a@]@[<v>%a@]@;" pp_player_list team.players
    pp_public_informations team.shared_public_informations

let pp_team_with_hand_of player fmt team =
  Format.fprintf fmt "@[<v>%a@]@[<v>%a@]@;"
    (pp_player_list_with_one_player_hand player)
    team.players pp_public_informations team.shared_public_informations

let pp_public_informations_list fmt pinfo_list =
  pinfo_list
  |> ((fun fmt ->
        List.iteri (fun i e ->
            Format.fprintf fmt "%d. @[<v>%a@]@;" i pp_public_informations e))
     |> Format.fprintf fmt "@[<v>%a@]")

let pp_names_of_team_list fmt teams =
  let pp_print_name_team fmt t =
    List.iter
      (fun p ->
        let p_struct = get_player_struct_from p in
        Format.fprintf fmt "%s;" p_struct.name)
      t.players
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
  let necessary_safety =
    match c with
    | Stop | SpeedLimit -> EmergencyVehicle
    | OutOfGas -> FuelTruck
    | FlatTire -> PunctureProof
    | Accident -> DrivingAce
  in
  has_already_used_safety_card p_info necessary_safety

let has_safety_to_counter_hazard_on_his_hand (p : player) (c : hazard_card) =
  let necessary_safety =
    match c with
    | Stop | SpeedLimit -> EmergencyVehicle
    | OutOfGas -> FuelTruck
    | FlatTire -> PunctureProof
    | Accident -> DrivingAce
  in
  let p_hand =
    match p with
    | Computer (p_struct, _) -> p_struct.hand
    | Human p_struct -> p_struct.hand
  in
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

let set_can_drive (t : team) (set_can_drive : bool) =
  {
    t with
    shared_public_informations =
      { t.shared_public_informations with can_drive = set_can_drive };
  }

let is_usable_hazard_card (p_info : public_informations) = function
  | SpeedLimit ->
      (not
         (has_safety_to_counter_hazard_on_public_informations p_info SpeedLimit))
      && not (is_attacked_by_speed_limit p_info)
  | hazard ->
      (not (has_safety_to_counter_hazard_on_public_informations p_info hazard))
      && not (is_attacked_by_hazard_on_drive_pile p_info)

let use_hazard_card (t : team) = function
  | SpeedLimit -> add_card_to_speed_limit_pile t (Hazard SpeedLimit)
  | Stop -> set_can_drive (add_card_to_drive_pile t (Hazard Stop)) false
  | hazard -> add_card_to_drive_pile t (Hazard hazard)

let is_usable_distance_card (p_info : public_informations) (c : distance_card) =
  if (not p_info.can_drive) || is_attacked_by_hazard_on_drive_pile p_info then
    false
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
  | EmergencyVehicle ->
      add_card_to_safety_area
        {
          t with
          shared_public_informations =
            { t.shared_public_informations with can_drive = true };
        }
        EmergencyVehicle
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
              can_drive = true;
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
  | Drive ->
      (not p_info.can_drive)
      &&
      if is_attacked_by_hazard_on_drive_pile p_info then
        peek_card_from_pile p_info.drive_pile = Hazard Stop
      else true
  | EndOfSpeedLimit -> is_attacked_by_speed_limit p_info
  | remedy ->
      (not (is_empty p_info.drive_pile))
      &&
      let hazard = get_hazard_corresponding_to_the_remedy remedy in
      peek_card_from_pile p_info.drive_pile = Hazard hazard
      && not (has_safety_to_counter_hazard_on_public_informations p_info hazard)

let use_remedy_card (t : team) = function
  | EndOfSpeedLimit -> add_card_to_speed_limit_pile t (Remedy EndOfSpeedLimit)
  | Drive -> set_can_drive (add_card_to_drive_pile t (Remedy Drive)) true
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
