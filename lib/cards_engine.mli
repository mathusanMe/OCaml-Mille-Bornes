(** This type is used to treat hazards. *)
type remedy_card = Drive | EndOfSpeedLimit | Gas | SpareTire | Repairs

(** This type is used to attack players. *)
type hazard_card = Stop | SpeedLimit | OutOfGas | FlatTire | Accident

(** This type is used to defend against hazards. *)
type safety_card = EmergencyVehicle | FuelTruck | PunctureProof | DrivingAce

(** This type is used to increase score. *)
type distance_card = D25 | D50 | D75 | D100 | D200

(** This type is a compilation of all possible card types in the game. *)
type card =
  | Safety of safety_card
  | Remedy of remedy_card
  | Hazard of hazard_card
  | Distance of distance_card

type deck_of_card = card list
(** A [deck_of_card] is used like a list of card. *)

type pile_of_card = card list
(** A [pile_of_card] is used like a stack of card. *)

val equal_remedy_card : remedy_card -> remedy_card -> bool
val equal_hazard_card : hazard_card -> hazard_card -> bool
val equal_safety_card : safety_card -> safety_card -> bool
val equal_distance_card : distance_card -> distance_card -> bool
val equal_card : card -> card -> bool
val equal_deck_of_card : deck_of_card -> deck_of_card -> bool
val equal_pile_of_card : pile_of_card -> pile_of_card -> bool
val pp_remedy : Format.formatter -> remedy_card -> unit
val pp_hazard : Format.formatter -> hazard_card -> unit
val pp_safety : Format.formatter -> safety_card -> unit
val pp_card : Format.formatter -> card -> unit
val pp_distance : Format.formatter -> distance_card -> unit
val pp_list_of_card : bool -> Format.formatter -> card list -> unit
val pp_deck_of_card : string -> Format.formatter -> deck_of_card -> unit
val pp_pile_of_card : string -> Format.formatter -> pile_of_card -> unit
val pp_top_pile_of_card : string -> Format.formatter -> pile_of_card -> unit

val generate_initial_pile : unit -> pile_of_card
(** [generate_initial_pile] generates an entire mille bornes [pile_of_card]. *)

exception Empty_pile
exception Empty_deck

val is_empty : card list -> bool
(** [is_empty card_list] returns true if [card_list] is empty. *)

val peek_card_from_pile : pile_of_card -> card
(** [peek_card_from_pile p] returns the first card of [p] without removing it from it.
    [raise Empty_pile] if [p] is empty. *)

val draw_card_from_pile : pile_of_card -> card * pile_of_card
(** [draw_card_from_pile p] draws a card from [] and returns the [card] and the new [pile_of_card].
    [raise Empty_pile] if [p] is empty. *)

val sort_card_list : card list -> card list
(** [sort_card_list card_list] sorts [card_list] using an order determinate in the engine. *)

val shuffle_pile : pile_of_card -> pile_of_card
(** [shuffle_pile p] shuffles the cards of [p] entirely. *)

val get_hazard_corresponding_to_the_remedy : remedy_card -> hazard_card
(** [get_hazard_corresponding_to_the_remedy r] returns the [hazard_card] which is treated by [r]. *)

val get_remedy_corresponding_to_the_hazard : hazard_card -> remedy_card
(** [get_remedy_corresponding_to_the_hazard h] returns the [remedy_card] that treat [h]. *)

val get_safety_corresponding_to_the_hazard : hazard_card -> safety_card
(** [get_safety_corresponding_to_the_hazard h] returns the [safety_card] that defends against [h]. *)

val add_card_to_pile : pile_of_card -> card -> pile_of_card
(** [add_card_to_pile p c] adds [c] at the head of [p]. *)

val add_card_to_deck : deck_of_card -> card -> deck_of_card
(** [add_card_to_deck d c] adds [c] at [d] which is then sorted by [sort_card_list] after that. *)

exception Card_not_found
(** Raised when a given card is not in a particular player's hand. *)

val remove_card_from_deck : deck_of_card -> card -> deck_of_card
(** [remove_card_from_deck d c] removes [c] from [d].
    [raise Empty_deck] if deck_of_card is empty. *)
