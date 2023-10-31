type remedy_card = Drive | EndOfSpeedLimit | Gas | SpareTire | Repairs
type hazard_card = Stop | SpeedLimit | OutOfGas | FlatTire | Accident
type safety_card = EmergencyVehicle | FuelTruck | PunctureProof | DrivingAce
type distance_card = D25 | D50 | D75 | D100 | D200

type card =
  | Remedy of remedy_card
  | Hazard of hazard_card
  | Safety of safety_card
  | Distance of distance_card

type deck_of_card = card list
(* A deck_of_card is used like a list *)

type pile_of_card = card list
(* A pile_of_card is used like a pile *)

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
val generate_initial_pile : unit -> pile_of_card
(* Generate an entire mille bornes card pile *)

val draw_card_from_pile : pile_of_card -> card * pile_of_card
(* Draw a card from a pile_of_card and return the card and the new pile_of_card *)

val sort_card_list : card list -> card list
(* Sort a cart list using an order determinate in the engine *)

val shuffle_pile : pile_of_card -> pile_of_card
(* Take a pile of card and shuffle it entirely *)

val get_hazard_corresponding_to_the_remedy : remedy_card -> hazard_card
val is_empty : card list -> bool
