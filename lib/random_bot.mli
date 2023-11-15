open Cards_engine
open Teams_engine

val random_strategy : strategy

val random_bot_choose_card_to_play :
  player ->
  public_informations ->
  public_informations list ->
  (int * int option) option

val random_bot_want_to_peek_discard_pile :
  player ->
  card ->
  public_informations ->
  public_informations list ->
  bool option

val random_bot_want_to_play_coup_fourre :
  player ->
  hazard_card ->
  public_informations ->
  public_informations list ->
  bool option
