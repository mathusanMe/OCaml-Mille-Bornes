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
type pile_of_card = card list

let remedy_to_string = function
  | Drive -> "Drive"
  | End_of_speed_limit -> "End of speed limit"
  | Gas -> "Gas"
  | Spare_tire -> "Spare tire"
  | Repairs -> "Repairs"

let hazard_to_string = function
  | Stop -> "Stop"
  | Speed_limit -> "Speed limit"
  | Out_of_gas -> "Out of gas"
  | Plat_tire -> "Plat tire"
  | Accident -> "Accident"

let safety_to_string = function
  | Emergency_vehicle -> "Emergency vehicle"
  | Fuel_truck -> "Fuel truck"
  | Puncture_proof -> "Puncture proof"
  | Driving_ace -> "Driving ace"

let distance_to_string = function
  | D25 -> "25"
  | D50 -> "50"
  | D75 -> "75"
  | D100 -> "100"
  | D200 -> "200"

let card_to_string = function
  | Remedy r -> remedy_to_string r
  | Hazard h -> hazard_to_string h
  | Safety s -> safety_to_string s
  | Distance d -> distance_to_string d
