open Cards_engine

type player_struct
(** This type is the structure representation without its [team] information such as [name] and playing [hand]. *)

type public_informations
(** This type represents what can be shared with other players, such as the [id], driving zone cards and [score]. *)

type player
(** Contains a [player_struct] and a [strategy] *)

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

type team
(** The [team] type is a representation of a game member, with 1 or 2 players in this implementation,
    its [public_informations] that they share between players, and the id of the [player] who must play the round. *)

exception Current_player_index_out_of_bound
(** Raised when the index of the current player in the team is outside the limits of the team list. *)

val get_current_player_from : team -> player
(** [get_current_player_from t] returns the [t] [player] with the same id as the [current_player_id] in [team].
    [raise Current_player_index_out_of_bound] if [current_player_id] is not valid *)

val set_hand_from : player -> deck_of_card -> player
(** [set_hand_from p d] returns [p] with the [hand] [d]. *)

val get_hand_from : player -> deck_of_card
(** [get_hand_from p d] returns [hand] of [player] [p]. *)

val get_name_from : player -> string
(** [get_name_from p] returns [name] of [player] [p]. *)

val get_strat_from : player -> strategy
(** [get_name_from p] returns [strategy] of [player] [p]. *)

val get_player_struct_from : player -> player_struct
(** [get_player_struct_from]returns [player_struct] of [player] [p]. *)

val have_same_contents_team : team -> team -> bool
(** [have_same_contents_team t1 t2] return if team [t1] equals [t2] (only used for testing) *)

val get_names_from : team -> string list
(** [get_names_from t] returns the names of all players in the [team], within a string list. *)

val get_players_from : team -> player list
(** [get_players_from t] returns the players list of [t]. *)

val get_public_informations_from : team -> public_informations
(** [get_public_informations_from t] returns public informations of [t]. *)

val get_current_player_id_from : team -> int
(** [get_current_player_id_from t] returns the current player id of [t]. *)

val get_id_from : public_informations -> int
(** [get_id_from pi] returns the id of [pi]. *)

val get_score_from : public_informations -> int
(** [get_score_from pi] returns the score of [pi]. *)

val get_speed_limit_pile_from : public_informations -> pile_of_card
(** [get_speed_limit_pile_from pi] returns the speed limit pile of the public information. *)

val get_drive_pile_from : public_informations -> pile_of_card
(** [get_drive_pile_from pi] returns the drive pile of the public information. *)

val get_distance_cards_from : public_informations -> deck_of_card
(** [get_distance_cards_from pi] returns the distance cards of the public information. *)

val get_safety_area_from : public_informations -> deck_of_card
(** [get_safety_area_from pi] returns the safety area of the public information. *)

val get_coup_fouree_cards_from : public_informations -> deck_of_card
(** [get_coup_fouree_cards_from pi] returns the coup fourre cards of the public information. *)

val set_next_player_from : team -> team
(** [set_next_player_from t] changes the id to the [player] who will play after the current [player], and loop if the id arrives at the end of the existing [players]. *)

val have_same_id_player : player -> player -> bool
(** [have_same_id_player p1 p2] returns true if the 2 [players] have the same [id]. *)

val does_player_have_this_name_in_team_list : string -> team list -> bool
(** [does_player_have_this_name_in_team_list name t_list] returns true if one of the players in the [t_list] has this [name]. *)

val same_team : team -> team -> bool
(** [same_team t1 t2] returns true if the [team] members have the same name, and share the same [public_informations]. *)

val replace_player_in : team -> player -> team
(** [replace_player_in t p] returns [t] with the replaced [p], by the function [have_same_id_player]. *)

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

exception Invalid_names
(** Raised when the same name is used several times*)

exception Not_valid_list_init_teams
(** Raised when a player give wrong name list and strat list to initialize a list of team. *)

exception Not_valid_names_init_teams
(** Raised when a player give a name list with similar names. *)

val init_teams : (string * strategy) list -> bool -> team list
(** [init_teams names strats teams_of_two] return a valid list of teams.
    [raise Not_valid_names_init_teams] if two names of [names] are similar.
    [raise Not_valid_list_init_teams] if lists don't have the same length, and if their length is not valid considering [teams_of_two]. *)

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

exception Index_of_hand_out_of_bound
(** Raised when the card index given for the hand is outside its limits. *)

val nth_hand_player : player -> int -> card
(** [nth_hand_player p i] returns the [card] to position [i] in the [p]'s hand.
    [raise Index_of_hand_out_of_bound] if [i] is not valid. *)

exception Team_already_have_hand
(** Raised if a user try to give an initial hand to a team who has already a hand. *)

val draw_initial_hand_to_team : team -> pile_of_card -> team * pile_of_card
(** [draw_initial_hand_to_team t p] gives each [player] in [t] six [card] in their hand draw from [pile].
    [raise Emtpy_pile] if the pile is too small. *)
