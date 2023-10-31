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

val get_current_player_from : team -> player
val get_player_struct_from : player -> player_struct
val set_next_player_from : team -> team
val is_computer : player -> bool
val init_team_with_one_player : string -> bool -> team
val init_team_with_two_players : string -> bool -> string -> bool -> team
val has_already_used_safety_card : team -> safety_card -> bool
