open Cards_engine

type player_struct = { name : string; hand : deck_of_card }
type player = Computer of player_struct | Human of player_struct

type public_informations = {
  speed_limit_pile : pile_of_card;
  drive_pile : pile_of_card;
  distance_cards : deck_of_card;
  safety_area : deck_of_card;
  coup_fouree_cards : deck_of_card;
  score : int;
  can_drive : bool;
}

type team = {
  players : player list;
  shared_public_informations : public_informations;
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
val is_usable_hazard_card : team -> hazard_card -> bool
val use_hazard_card : team -> hazard_card -> team
val is_usable_distance_card : team -> distance_card -> bool
val use_distance_card : team -> distance_card -> team
val is_usable_safety_card : team -> safety_card -> bool
val use_safety_card : team -> safety_card -> team
val use_coup_fouree : team -> safety_card -> team
val is_usable_remedy_card : team -> remedy_card -> bool
val use_remedy_card : team -> remedy_card -> team
val is_usable_card : team -> card -> bool
val use_card : team -> card -> team
