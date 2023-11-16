open Cards_engine

type player_struct = { name : string; hand : deck_of_card }
(** This type is the structure representation without its [team] information such as [name] and playing [hand]. *)

type public_informations = {
  id : int;
  speed_limit_pile : pile_of_card;
  drive_pile : pile_of_card;
  distance_cards : deck_of_card;
  safety_area : deck_of_card;
  coup_fouree_cards : deck_of_card;
  score : int;
}
(** This type represents what can be shared with other players, such as the [id], driving zone cards and [score]. *)

(** This type makes the difference between a [Human] and a [Bot], with a [Bot] that also has a defined game [strategy]. *)
type player = Computer of (player_struct * strategy) | Human of player_struct

and strategy = {
  name : string;  (** Name of the Computer strategy. *)
  choose_card_to_play :
    player ->
    public_informations ->
    public_informations list ->
    (int * int option) option;
      (** Method that takes the [player] who will play (the computer),
         the [public_informations] of the [team] of the same [player], as well
         as a list with [public_informations] of all other teams that play.
         And returns a pair composed of an int corresponding to the index
         of the [card] it wants to play in its deck, as well as the identifier
         of the [public_informations] it wants to use its [card]. The int option type for
         [team] identification is present because it may want to deflect a [card].
         In this case it will have to return `None'. If it want to use a [card] on
         its own [public_informations] it will have to use the right id (second
         argument of the function). *)
  want_to_peek_discard_pile :
    player ->
    card ->
    public_informations ->
    public_informations list ->
    bool option;
      (** Method that takes the [player] who will play (the computer),
        the [card] present on the top of the discard pile,
        the [public_informations] of the [team] of the same [player], as well as
        a list with [public_informations] of all the teams that play.
        And returns a boolean indicating whether it wants to take
        the [card] at the top of the discard pile, or whether it wants
        to take the [card] above the draw pile. *)
  want_to_play_coup_fourre :
    player ->
    hazard_card ->
    public_informations ->
    public_informations list ->
    bool option;
      (** When a [team] is attacked, this method is called if the [team]
        can play a coup fourre. This method takes the [player] who will
        play (the computer), the [card] by which it is attacked,
        the [public_informations] of the [team] of the same [player], as well
        as a list with [public_informations] of all the teams that play.
        And returns a boolean indicating if it wants to make a coup fourre. *)
}
(** This type is composed of several functions that allow the [Bot] to produce game actions. *)

type team = {
  players : player list;
  shared_public_informations : public_informations;
  current_player_index : int;
}
(** The [team] type is a representation of a game member, with 1 or 2 players in this implementation,
    its [public_informations] that they share between players, and the id of the [player] who must play the round. *)

val get_current_player_from : team -> player
(** [get_current_player_from t] returns the [t] [player] with the same id as the [current_player_id] in [team]. *)

val get_player_struct_from : player -> player_struct
(** [get_player_struct_from p] returns the structure of [p], whether it's a [Bot] or a classic [player]. *)

val get_names_from : team -> string list
(** [get_names_from t] returns the names of all players in the [team], within a string list. *)

val set_next_player_from : team -> team
(** [set_next_player_from t] changes the id to the [player] who will play after the current [player], and loop if the id arrives at the end of the existing [players]. *)

val same_player : player -> player -> bool
(** [same_player p1 p2] returns true if the 2 [players] have the same name, because in a game, 2 [players] are not allowed to have the same name. *)

val does_player_have_this_name_in_team_list : string -> team list -> bool
(** [does_player_have_this_name_in_team_list name t_list] returns true if one of the players in the [t_list] has this [name]. *)

val same_team : team -> team -> bool
(** [same_team t1 t2] returns true if the [team] members have the same name, and share the same [public_informations]. *)

val replace_player_struct_in : player -> player_struct -> player
(** [replace_player_struct_in p p_struct] returns [p] with the new structure [p_struct]. *)

val replace_player_in : team -> player -> team
(** [replace_player_struct_in t p] returns [t] with the replaced [p], by the function [same_player]. *)

val replace_team_in : team list -> team -> team list
(** [replace_team_in t_list t] returns [t_list] with the replaced [t], by the function [same_team]. *)

val pp_player : bool -> Format.formatter -> player -> unit

val pp_public_informations :
  bool -> Format.formatter -> public_informations -> unit

val pp_team : bool -> bool -> Format.formatter -> team -> unit
val pp_team_with_hand_of : player -> Format.formatter -> team -> unit

val pp_public_informations_list :
  Format.formatter -> public_informations list -> unit

val pp_names_of_team_list : Format.formatter -> team list -> unit

val init_team_with_one_human : string -> int -> team
(** [init_team_with_one_human name id] initializes a [team] consisting of one [Human] [player] with his [name] and [team] [id]. *)

val init_team_with_one_computer : string -> strategy -> int -> team
(** [init_team_with_one_computer name s id] initializes a [team] consisting of one [Computer] player with its [name], its [strategy] [s] and its [team] [id]. *)

val init_team_with_two_human : string -> string -> int -> team
(** [init_team_with_two_human name1 name2 id] initializes a [team] consisting of two [Human] [players] with respectively the names [name1] and [name2], and their shared
    [team] [id]. *)

val init_team_with_one_human_and_one_computer :
  string -> bool -> string -> bool -> strategy -> int -> team
(** [init_team_with_one_human_and_one_computer name1 is_compute1 name2 is_computer2 s id] initializes a [team] of one [Human] and one [Computer],
    with their order decided by [is_bot1] and [is_bot2], their respective names [name1] and [name2], the [Bot]'s [strategy], and their shared [team] [id]. *)

val init_team_with_two_computer :
  string -> strategy -> string -> strategy -> int -> team
(** [init_team_with_two_computer name1 s1 name2 s2 id] initializes a [team] of two [Computer], with their respective names [name1] and [name2], 
    their respective [strategy] [s1] and [s2], and their shared [team] [id]. *)

val is_attacked_by_hazard_on_drive_pile : public_informations -> bool
(** [is_attacked_by_hazard_on_drive_pile pi] checks the top of the drive pile of [pi] and returns true if it contains a [hazard_card]. *)

val is_attacked_by_speed_limit : public_informations -> bool
(** [is_attacked_by_speed_limit pi] checks the top of the speed limit pile of [pi] and returns true if it contains a [hazard_card]. *)

val is_card_in_player_hand : player -> card -> bool
(** [is_card_in_player_hand player c] returns true if a [c] is equal to at least one in the hand deck of [player]. *)

val use_card : team -> card -> team
(** [use_card t c] applies the [card]'s effect of [c] and places [c] at the right place in the [t]'s driving zone. *)

val use_coup_fouree : team -> safety_card -> team
(** [use_coup_fouree t s] adds the [safety_card] [s] to the [team]'s coup fourre pile and gives them 200 points, according to the rules. *)

val has_safety_to_counter_hazard_on_his_hand : player -> hazard_card -> bool
(** [has_safety_to_counter_hazard_on_his_hand p h] checks if [p] does not have a [safety_card] in his hand to defend against [hazard_card] and returns true if so. *)

val is_usable_card : public_informations -> card -> bool
(** [is_usable_card pi c] returns true if [c] can be used on the [pi]'s driving zone, according to the game rules. *)

val nth_hand_player : player -> int -> card
(** [nth_hand_player p i] returns the [card] to position [i] in the [p]'s hand. *)
