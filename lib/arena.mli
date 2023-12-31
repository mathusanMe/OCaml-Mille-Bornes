open Cards_engine
open Teams_engine
open Board_engine

(** This type represents the end result of a game, with a winning [team] composed of their names,
    a [team] dropping out with their name, a dropout during game initialization, or an error in the code. *)
type endplay =
  | Equality
  | Win of string list
  | GiveUpInGame of string list
  | GiveUpInit
  | Error of string

val pp_endplay : Format.formatter -> endplay -> unit

val has_win : team -> bool
(** [has_win t] returns true if a [team] has over 1000 in score, and has therefore won the game. *)

val get_strategy_list : unit -> strategy list
(** [get_strategy_list] returns a list of all possible [Bot] [strategy] in the game. *)

val player_teletype_choose_card_to_play :
  player ->
  public_informations ->
  public_informations list ->
  (int * int option) option
(** [player_teletype_choose_card_to_play_card p pi pi_list] delegates the choice
    of move to the [player] from the I/O terminal with [pi] its [public_informations],
    [pi_list] that of the others, and returns an integer pair with the 1st being the
    position of its [card] in its hand, and the 2nd the [team] it is going to attack
    and finally returns None if it doesn't use it against a [team], but discard
    instead. *)

val player_teletype_want_to_peek_discard_pile :
  player ->
  card ->
  public_informations ->
  public_informations list ->
  bool option
(** [player_teletype_want_to_peek_discard_pile p c pi pi_list] delegates the choice
    of whether the [player] wants to draw the discard pile [card] if true, or the draw
    pile [card] if false with [c] the [card] on top of the discard pile, [pi] its
    [public_informations] and [pi_list] that of the other [player]. *)

val player_teletype_want_to_play_coup_fourre :
  player ->
  hazard_card ->
  public_informations ->
  public_informations list ->
  bool option
(** [player_teletype_want_to_play_coup_fourre p h pi pi_list] delegates the choice
    of wheter or not [p] wants to play the coup fourre from the I/O terminal
    with [h] the [card] attacking it, [pi] its [public_informations], and [pi_list] that of
    the others. *)

val pp_strategy_list : Format.formatter -> strategy list -> unit

val ask_teams_info :
  unit -> ((string * Teams_engine.strategy) list * bool) option
(** [ask_teams_info] initializes all name and strategy, and if the teams contains 1 or 2 players, to start a party with the terminal I/O commands. *)

val play_move_player : int -> board -> board option
(** [play_move_player t b] simulates the current [player]'s move on [b], and returns
    [b] changed. You have [t] attempts to successfully play a valid move.*)

val arena : unit -> endplay
(** [arena] simulates a game for a [player] initialized at the start of the
    function, and returns the result of the game. *)
