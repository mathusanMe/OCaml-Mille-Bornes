open Cards_engine
open Teams_engine

type board = {
  draw_pile : pile_of_card;
  discard_pile : pile_of_card;
  teams : team list;
  current_team_index : int;
}

val get_current_team_from : board -> team

exception Team_not_found

val draw_card : board -> team -> board
(* [draw_card b t] draws a card from the draw pile and adds it to the hand of
 * the current player on team [t]. If the draw pile is empty, raise EmptyPile. *)

val is_draw_pile_empty : board -> bool
val is_discard_pile_empty : board -> bool
val swap_draw_and_shuffled_discard_pile : board -> board
(* [switch_draw_and_discard_pile b] returns the board with the draw pile and discard pile swapped, and the draw pile shuffled.*)
