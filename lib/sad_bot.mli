open Cards_engine
open Teams_engine

exception Hazard_on_the_drive_pile_missing

val sad_bot_choose_card_to_play :
  player ->
  public_informations ->
  public_informations list ->
  (int * int option) option

val sad_bot_want_to_draw_discard_pile :
  player ->
  card ->
  public_informations ->
  public_informations list ->
  bool option

val sad_bot_want_to_play_coup_fourre :
  player ->
  hazard_card ->
  public_informations ->
  public_informations list ->
  bool option
