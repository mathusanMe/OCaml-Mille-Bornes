open Cards_engine

type player_struct = { name : string; hand : deck_of_card }
type player = Computer of player_struct | Human of player_struct

type driving_zone = {
  speed_limit_pile : pile_of_card;
  drive_pile : pile_of_card;
  can_drive : bool;
  distance_cards : deck_of_card;
  score : int;
  safety_area : deck_of_card;
  coup_fouree_cards : deck_of_card;
}

type team = {
  players : player list;
  shared_driving_zone : driving_zone;
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
    can_drive = false;
    distance_cards = [];
    score = 0;
    safety_area = [];
    coup_fouree_cards = [];
  }

let init_team_with_one_player (name : string) (is_computer : bool) =
  let player = if is_computer then init_computer name else init_human name in
  {
    players = [ player ];
    shared_driving_zone = init_driving_zone ();
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
      current_player_index = 1 - t.current_player_index;
    }
  else t

let is_computer = function Computer _ -> true | Human _ -> false

let has_already_used_safety_card (t : team) (c : safety_card) =
  let f = List.exists (fun x -> x = Safety c) in
  f t.shared_driving_zone.safety_area
  || f t.shared_driving_zone.coup_fouree_cards
