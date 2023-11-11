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
  (*name of the Computer strategy*)
  choose_card_to_play :
    player ->
    public_informations ->
    public_informations list ->
    int * int option;
      (*Method that takes the player who will play (the computer),
        the public_information of the team of the same player, as well
        as a list with public_information of all other teams that play.
        And returchoose_card_to_playns a pair composed of an int corresponding to the index
        of the card it wants to play in its deck, as well as the identifier
        of the public_information it wants to use its card. The int option type for
        team identification is present because it may want to deflect a card.
        In this case it will have to return `None'. If it want to use a card on
        its own public_information it will have to use the right id (second
        argument of the function).*)
  want_to_peek_discard_pile :
    player -> card -> public_informations -> public_informations list -> bool;
      (*Method that takes the player who will play (the computer),
        the card present on the top of the discard pile,
        the public_information of the team of the same player, as well as
        a list with public_information of all the teams that play.
        And returns a boolean indicating whether it wants to take
        the card at the top of the discard pile, or whether it wants
        to take the card above the draw pile.*)
  want_to_play_coup_fourre :
    player ->
    hazard_card ->
    public_informations ->
    public_informations list ->
    bool;
      (*When a team is attacked, this method is called if the team
        can play a coup fourre. This method takes the player who will
        play (the computer), the card by which it is attacked,
        the public_information of the team of the same player, as well
        as a list with public_information of all the teams that play.
        And returns a boolean indicating if it wants to make a coup fourre.*)
}

type team = {
  players : player list;
  shared_public_informations : public_informations;
  current_player_index : int;
}

val get_current_player_from : team -> player
val get_player_struct_from : player -> player_struct
val set_next_player_from : team -> team
val same_player : player -> player -> bool
val same_team : team -> team -> bool
val replace_player_struct_in : player -> player_struct -> player
val replace_player_in : team -> player -> team
val replace_team_in : team list -> team -> team list
val pp_player : bool -> Format.formatter -> player -> unit
val pp_team : bool -> Format.formatter -> team -> unit
val init_team_with_one_human : string -> int -> team
val init_team_with_one_computer : string -> strategy -> int -> team
val init_team_with_two_human : string -> string -> int -> team

val init_team_with_one_human_and_one_computer :
  string -> bool -> string -> bool -> strategy -> int -> team

val init_team_with_two_computer :
  string -> strategy -> string -> strategy -> int -> team

val use_card : team -> card -> team
val use_coup_fouree : team -> safety_card -> team
val has_safety_to_counter_hazard_on_his_hand : player -> hazard_card -> bool
val is_usable_card : public_informations -> card -> bool
