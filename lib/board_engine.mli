open Cards_engine
open Teams_engine

type board = {
  draw_pile : pile_of_card;
  discard_pile : pile_of_card;
  teams : team list;
  current_team_index : int;
}

val find_index : ('a -> bool) -> 'a list -> int option
(* [find_index a l] returns the index of [a] in [l]. If [a] is not in [l], returns None *)

val get_index_of_card_on_hand : card -> player -> int
val get_current_team_from : board -> team

exception Team_not_found

val draw_card : board -> team -> board
(* [draw_card b t] draws a card from the draw pile and adds it to the hand of
 * the current player on team [t]. If the draw pile is empty, raise Empty_pile. *)

val is_draw_pile_empty : board -> bool
val is_discard_pile_empty : board -> bool
val swap_draw_and_shuffled_discard_pile : board -> board
(* [switch_draw_and_discard_pile b] returns the board with the draw pile and discard pile swapped, and the draw pile shuffled.*)

exception Card_not_found

val discard_card : board -> team -> card -> board
(* [discard_card b t c] discards card [c] from the hand of the current player
 * on team [t] and adds it to the discard pile. If the card is not in the hand
 * of the current player, raise CardNotFound. If the team is not found, raise
 * TeamNotFound. *)

val discard_card_from_player : board -> team -> player -> card -> board
(* [discard_card b t p c] discards card [c] from the hand of [p] player
 * on team [t] and adds it to the discard pile. If the card is not in the hand
 * of the [p] player, raise CardNotFound. If the team is not found, raise
 * TeamNotFound. If the player is not found on the team, raise PlayerNotFound*)

exception Invalid_move
exception Unusable_card

val place_card : board -> team -> card -> team -> board
(* [place_card b t1 c t2] places card [c] from the hand of the current player
 * on team [t1] and places it on the driving zone of team [t2]. If the card is
 * not in the hand of the current player, raise CardNotFound. If the team is
 * not found, raise TeamNotFound. *)

exception Player_not_found

val set_previous_current_team_from : board -> team -> board
(* [set_previous_current_team_from b t] take a board [b] and a team [t]
 * and return a new board with the team that normally precedes [t] as 
 * current team index*)

val place_coup_fouree : board -> team -> player -> safety_card -> board
(* [place_coup_fouree b t p c] places safety card [c] from the hand of the 
 * [p] player on team [t] and places it on the driving zone of team [t]. 
 * If the card is not in the hand of the current player, raise CardNotFound. 
 * If the team is not found, raise TeamNotFound. If the player is
 * not found on the team, raise PlayerNotFound*)
