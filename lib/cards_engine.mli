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

val card_to_string : card -> string
val generate_initial_pile : unit -> pile_of_card
(* Generate an entire mille bornes card pile *)
