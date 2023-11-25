open Cards_engine
open Teams_engine

type board = {
  draw_pile : pile_of_card;
  discard_pile : pile_of_card;
  teams : team list;
  current_team_index : int;
}
(** This type represents a board with all the teams on it, and the game decks, i.e. the discard pile and draw pile. *)

val find_index : ('a -> bool) -> 'a list -> int option
(** [find_index a l] returns the index of [a] in [l]. If [a] is not in [l], returns None. *)

val get_index_of_card_on_hand : card -> player -> int
(** [get_index_of_card_on_hand c p] returns the index of the first [card] in the hand deck of [p] corresponding to [c]. *)

exception Current_team_index_out_of_bound
(** Raised when the index of the current team in board is outside the limits of the team list.  *)

val get_current_team_from : board -> team
(** [get_current_team_from b] returns the [team] with the [current_team_index] id of [b]. *)

val switch_current_player_of_current_team_from : board -> board
(** [switch_current_player_of_current_team_from] changes the current [player] in the current [team] to the next current [player] in the same [team]. *)

val switch_current_team_from : board -> board
(** [switch_current_team_from b] changes the current [team] to the next in the [team] list of [b]. *)

exception No_more_card
(* Raised when draw pile and discard pile is empty *)

exception Draw_pile_too_small
(** Raised when a draw pile is too small to distribute a deck to all [player] on a [board]. *)

val draw_initial_hand_to_teams : board -> board
(** [draw_initial_hand_to_teams b] gives each [player] in every [team] on the [board] six [card] in their hand draw from the [draw_pile] of [b]. *)

val draw_card : board -> team -> bool -> board
(** [draw_card b t from_discard_pile] draws a [card] from a pile depending on [from_discard_pile] and adds it to the hand of
    the current [player] on team [t].
    [raise Empty_pile] if the pile drawed is empty. *)

val is_draw_pile_empty : board -> bool
val is_discard_pile_empty : board -> bool

val swap_draw_and_shuffled_discard_pile : board -> board
(** [switch_draw_and_discard_pile b] returns [b] with the draw pile and discard pile swapped, and the draw pile shuffled. *)

exception Card_not_found
(** Raised when a given card is not in a particular player's hand. *)

exception Team_not_found
(** Raised when a wrong team not belonging to a given [board]. *)

exception Player_not_found
(** Raised when a wrong player not belonging to a given [team]. *)

val discard_card : board -> team -> card -> board
(** [discard_card b t c] discards card [c] from the hand of the current [player]
    of [team] [t] and adds it to the discard pile.
    [raise Card_not_found] if [c] is not in the hand of the current [player].
    [raise Team_not_found] if [t] is not found. *)

val discard_card_from_player : board -> team -> player -> card -> board
(** [discard_card b t p c] discards card [c] from the hand of [p] [player]
    on [team] [t] and adds it to the discard pile. 
    [raise Card_not_found] if the [card] [c] is not in the hand of the [p] player. 
    [raise Team_not_found] if the [team] [t] is not found. 
    [raise Player_not_found] if the [player] [p] is not found on the [team]. *)

exception Invalid_move
(** Raised when a [card] sent by a [player] cannot be applied to a particular target [player]. *)

exception Unusable_card
(** Raised when a [card] cannot be used with [use_card] on a [team]'s driving zone*)

val place_card : board -> team -> card -> team -> board
(** [place_card b t1 c t2] places [card] [c] from the hand of the current [player]
    on [team] [t1] and places it on the driving zone of [team] [t2].
    [raise Card_not_found] if the [card] [c] is not in the hand of the current [player].
    [raise Team_not_found] if the [team] [t1] or [t2] is not found. *)

val set_previous_current_team_from : board -> team -> board
(** [set_previous_current_team_from b t] take a board [b] and a team [t]
    and returns a new board with the team that normally precedes [t] as 
    current [team] index. *)

val place_coup_fouree : board -> team -> player -> safety_card -> board
(** [place_coup_fouree b t p c] places [safety_card] [c] from the hand of the 
    [p] [player] on [team] [t] and places it on the driving zone of [team] [t]. 
    [raise Card_not_found] if the [card] [c] is not in the hand of the current [player].
    [raise Team_not_found] if [t] is not found in [b]. 
    [raise Player_not_found] if [p] is not found on the [t] *)
