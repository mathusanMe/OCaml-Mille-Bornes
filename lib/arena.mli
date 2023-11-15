open Cards_engine
open Teams_engine
open Board_engine

type endplay = Win of team | GiveUpInGame of team | GiveUpInit

val pp_endplay : Format.formatter -> endplay -> unit

val player_teletype_choose_card_to_play :
  player ->
  public_informations ->
  public_informations list ->
  (int * int option) option
(* [player_teletype_choose_card_to_play_card p pi pi_list] delegates the choice
   of move to the player from the I/O terminal with pi its public information,
   pi_list that of the others, and returns an integer pair with the 1st being the
   position of its card in its hand, and the 2nd the player it is going to attack
   (returns None if it doesn't use it against a player, but discard
   instead). *)

val player_teletype_want_to_peek_discard_pile :
  player ->
  card ->
  public_informations ->
  public_informations list ->
  bool option
(* [player_teletype_want_to_peek_discard_pile p c pi pi_list] delegates the choice
   of whether the player wants to draw the discard pile card if true, or the draw
   pile card if false with c the card on
   top of the discard pile, pi its public information and pi_list that of the other
   players. *)

val player_teletype_want_to_play_coup_fourre :
  player ->
  hazard_card ->
  public_informations ->
  public_informations list ->
  bool option
(* [player_teletype_want_to_play_coup_fourre p h pi pi_list] delegates the choice
   of wheter or not the player wants to play the coup fourre from the I/O terminal
   with h the card attacking it, pi its public_information, and pi_list that of the
   others. *)

val pp_strategy_list : Format.formatter -> strategy list -> unit
val init_teams : unit -> team list option
(* [init_teams] initializes all team of the board with the terminal I/O commands *)

val init_board : unit -> board option
(* [init_board] initializes the board with all teams created with the terminal
   I/O commands and generated piles*)

val play_move_player : board -> board option
(* [play_move_player b] simulates the current player's move on a board, and returns
   the changed board. *)

val arena : unit -> unit
(* [arena] simulates a game for a player initialized at the start of the
   function, and returns the result of the game. *)
