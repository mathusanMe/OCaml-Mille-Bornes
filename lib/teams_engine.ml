open Cards_engine

type player_struct = { name : string; hand : deck_of_card }
type player = Computer of player_struct | Human of player_struct

type driving_zone = {
  speed_limit_pile : pile_of_card;
  drive_pile : pile_of_card;
  distance_cards : deck_of_card;
  safety_area : deck_of_card;
  coup_fouree_cards : deck_of_card;
}

type team = {
  players : player list;
  shared_driving_zone : driving_zone;
  score : int;
  can_drive : bool;
  current_player_index : int;
}

let init_player_struct (entered_name : string) =
  { name = entered_name; hand = [] }

let init_human (entered_name : string) = Human (init_player_struct entered_name)
let init_computer (name : string) = Computer (init_player_struct name)

let init_driving_zone () =
  {
    speed_limit_pile = [];
    drive_pile = [];
    distance_cards = [];
    safety_area = [];
    coup_fouree_cards = [];
  }

let init_team_with_one_player (name : string) (is_computer : bool) =
  let player = if is_computer then init_computer name else init_human name in
  {
    players = [ player ];
    shared_driving_zone = init_driving_zone ();
    score = 0;
    can_drive = false;
    current_player_index = 0;
  }

let init_team_with_two_players (name1 : string) (is_computer1 : bool)
    (name2 : string) (is_computer2 : bool) =
  let player1 =
    if is_computer1 then init_computer name1 else init_human name1
  in
  let player2 =
    if is_computer2 then init_computer name2 else init_human name2
  in
  {
    players = [ player1; player2 ];
    shared_driving_zone = init_driving_zone ();
    score = 0;
    can_drive = false;
    current_player_index = 0;
  }

let get_current_player_from (t : team) =
  List.nth t.players t.current_player_index

let get_player_struct_from (p : player) =
  match p with Human e -> e | Computer e -> e

let set_next_player_from (t : team) =
  if List.length t.players = 2 then
    {
      players = t.players;
      shared_driving_zone = t.shared_driving_zone;
      score = 0;
      can_drive = false;
      current_player_index = 1 - t.current_player_index;
    }
  else t

let is_computer = function Computer _ -> true | Human _ -> false

let same_player (p1 : player) (p2 : player) =
  let e1 = get_player_struct_from p1 in
  let e2 = get_player_struct_from p2 in
  e1.name = e2.name

let same_team (t1 : team) (t2 : team) =
  List.for_all2 same_player t1.players t2.players

let replace_player_struct_in (p : player) (p_struct : player_struct) =
  match p with Computer _ -> Computer p_struct | Human _ -> Human p_struct

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
  | Computer p_struct ->
      Format.fprintf fmt "%s (computer)@ " p_struct.name;
      pp_hand with_hand p_struct.hand
  | Human p_struct ->
      Format.fprintf fmt "%s@ " p_struct.name;
      pp_hand with_hand p_struct.hand

let pp_player_list with_hands fmt players =
  let pp_iter fmt =
    List.iter (fun p -> Format.fprintf fmt "%a" (pp_player with_hands) p)
  in
  if with_hands then
    Format.fprintf fmt "Name(s) with deck :@ %a" pp_iter players
  else Format.fprintf fmt "Name(s) :@ %a" pp_iter players

let pp_driving_zone fmt dz =
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
  Format.fprintf fmt "@[<v>%a@]@[<v>%a@]@;" (pp_player_list with_hand)
    team.players pp_driving_zone team.shared_driving_zone

let has_already_used_safety_card (t : team) (c : safety_card) =
  let f = List.exists (fun x -> x = Safety c) in
  f t.shared_driving_zone.safety_area
  || f t.shared_driving_zone.coup_fouree_cards

let has_safety_to_counter_hazard (t : team) (c : hazard_card) =
  let necessary_safety =
    match c with
    | Stop | SpeedLimit -> EmergencyVehicle
    | OutOfGas -> FuelTruck
    | FlatTire -> PunctureProof
    | Accident -> DrivingAce
  in
  has_already_used_safety_card t necessary_safety

let is_attacked_by_hazard_on_drive_pile (t : team) =
  (not (is_empty t.shared_driving_zone.drive_pile))
  &&
  match List.hd t.shared_driving_zone.drive_pile with
  | Hazard hazard -> not (has_safety_to_counter_hazard t hazard)
  | _ -> false

let is_attacked_by_speed_limit (t : team) =
  (not (is_empty t.shared_driving_zone.speed_limit_pile))
  && List.hd t.shared_driving_zone.speed_limit_pile = Hazard SpeedLimit
  && not (has_safety_to_counter_hazard t SpeedLimit)

let add_card_to_speed_limit_pile (t : team) (c : card) =
  {
    t with
    shared_driving_zone =
      {
        t.shared_driving_zone with
        speed_limit_pile =
          add_card_to_pile t.shared_driving_zone.speed_limit_pile c;
      };
  }

let add_card_to_drive_pile (t : team) (c : card) =
  {
    t with
    shared_driving_zone =
      {
        t.shared_driving_zone with
        drive_pile = add_card_to_pile t.shared_driving_zone.drive_pile c;
      };
  }

let set_can_drive (t : team) (set_can_drive : bool) =
  { t with can_drive = set_can_drive }

let is_usable_hazard_card (t : team) = function
  | SpeedLimit ->
      (not (has_safety_to_counter_hazard t SpeedLimit))
      && not (is_attacked_by_speed_limit t)
  | hazard ->
      (not (has_safety_to_counter_hazard t hazard))
      && not (is_attacked_by_hazard_on_drive_pile t)

let use_hazard_card (t : team) = function
  | SpeedLimit -> add_card_to_speed_limit_pile t (Hazard SpeedLimit)
  | Stop -> set_can_drive (add_card_to_drive_pile t (Hazard Stop)) false
  | hazard -> add_card_to_drive_pile t (Hazard hazard)
