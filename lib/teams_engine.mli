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
val is_usable_card : public_informations -> card -> bool
val use_card : team -> card -> team
val use_coup_fouree : team -> safety_card -> team
