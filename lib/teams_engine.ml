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
