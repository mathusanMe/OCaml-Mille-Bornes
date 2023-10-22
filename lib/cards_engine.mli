type remedy_card = Drive | End_of_speed_limit | Gas | Spare_tire | Repairs
type hazard_card = Stop | Speed_limit | Out_of_gas | Plat_tire | Accident

type safety_card =
  | Emergency_vehicle
  | Fuel_truck
  | Puncture_proof
  | Driving_ace

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
