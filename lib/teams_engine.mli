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

val get_current_player_from : team -> player
val get_player_struct_from : player -> player_struct
val set_next_player_from : team -> team
val is_computer : player -> bool
val same_player : player -> player -> bool
val same_team : team -> team -> bool
val replace_player_struct_in : player -> player_struct -> player
val replace_player_in : team -> player -> team
val replace_team_in : team list -> team -> team list
val pp_player : bool -> Format.formatter -> player -> unit
val pp_team : bool -> Format.formatter -> team -> unit
val init_team_with_one_player : string -> bool -> team
val init_team_with_two_players : string -> bool -> string -> bool -> team
val has_already_used_safety_card : team -> safety_card -> bool
val has_safety_to_counter_hazard : team -> hazard_card -> bool
val is_attacked_by_hazard_on_drive_pile : team -> bool
val is_attacked_by_speed_limit : team -> bool

(* TODO : removed from the .mli add_card_to_speed_limit_pile and add_card_to_drive_pile_and_set_can_drive*)
val add_card_to_speed_limit_pile : team -> card -> team
val add_card_to_drive_pile : team -> card -> team
val add_card_to_drive_pile_and_set_can_drive : team -> card -> bool -> team
