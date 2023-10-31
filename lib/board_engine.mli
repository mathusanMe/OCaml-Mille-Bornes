open Cards_engine
open Teams_engine

type board = {
  draw_pile : pile_of_card;
  discard_pile : pile_of_card;
  teams : team list;
  current_team_index : int;
}

val get_current_team_from : board -> team

exception TeamNotFound

val draw_card : board -> team -> board
(* [draw_card b t] draws a card from the draw pile and adds it to the hand of
 * the current player on team [t]. If the draw pile is empty, raise EmptyPile. *)