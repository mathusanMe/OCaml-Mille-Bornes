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

let init_card_from_int = function
  | n when n <= 0 -> Safety Emergency_vehicle
  | n when n <= 1 -> Safety Fuel_truck
  | n when n <= 2 -> Safety Puncture_proof
  | n when n <= 3 -> Safety Driving_ace
  | n when n <= 8 -> Hazard Stop
  | n when n <= 12 -> Hazard Speed_limit
  | n when n <= 15 -> Hazard Out_of_gas
  | n when n <= 18 -> Hazard Plat_tire
  | n when n <= 21 -> Hazard Accident
  | n when n <= 35 -> Remedy Drive
  | n when n <= 41 -> Remedy End_of_speed_limit
  | n when n <= 47 -> Remedy Gas
  | n when n <= 53 -> Remedy Spare_tire
  | n when n <= 59 -> Remedy Repairs
  | n when n <= 69 -> Distance D25
  | n when n <= 79 -> Distance D50
  | n when n <= 89 -> Distance D75
  | n when n <= 101 -> Distance D100
  | _ -> Distance D200

let generate_initial_pile : unit -> pile_of_card =
 fun () -> List.init 106 init_card_from_int
