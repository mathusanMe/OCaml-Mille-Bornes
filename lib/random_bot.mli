open Cards_engine
open Teams_engine

val random_strategy : strategy
(** Represents the [strategy] with all function of the random bot. *)

val random_bot_choose_card_to_play :
  player ->
  public_informations ->
  public_informations list ->
  (int * int option) option
(** [random_bot_choose_card_to_play p pi pi_list] with the information given, returns the possible 
    random [card] id in its hand, and the id of itself if it's a remedy, scoring or safety [card], 
    another if it's an hazard card, or None for discard, and all None for give up. *)

val random_bot_want_to_peek_discard_pile :
  player ->
  card ->
  public_informations ->
  public_informations list ->
  bool option
(** [random_bot_want_to_peek_discard_pile p c pi pi_list] with available information, returns true if it wants
    to take a [card] in the discard pile, false in the draw pile, and None if it wants to surrender. *)

val random_bot_want_to_play_coup_fourre :
  player ->
  hazard_card ->
  public_informations ->
  public_informations list ->
  bool option
(** [random_bot_want_to_play_coup_fourre p h pi pi_list] with the information available, returns true if it wants
    to do a trick, false if not, and None if it wants to give up. *)
