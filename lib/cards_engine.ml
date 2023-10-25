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
type pile_of_card = card list

let remedy_to_string = function
  | Drive -> "Drive"
  | EndOfSpeedLimit -> "End of speed limit"
  | Gas -> "Gas"
  | SpareTire -> "Spare tire"
  | Repairs -> "Repairs"

let hazard_to_string = function
  | Stop -> "Stop"
  | SpeedLimit -> "Speed limit"
  | OutOfGas -> "Out of gas"
  | FlatTire -> "Plat tire"
  | Accident -> "Accident"

let safety_to_string = function
  | EmergencyVehicle -> "Emergency vehicle"
  | FuelTruck -> "Fuel truck"
  | PunctureProof -> "Puncture proof"
  | DrivingAce -> "Driving ace"

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
  | n when n <= 0 -> Safety EmergencyVehicle
  | n when n <= 1 -> Safety FuelTruck
  | n when n <= 2 -> Safety PunctureProof
  | n when n <= 3 -> Safety DrivingAce
  | n when n <= 8 -> Hazard Stop
  | n when n <= 12 -> Hazard SpeedLimit
  | n when n <= 15 -> Hazard OutOfGas
  | n when n <= 18 -> Hazard FlatTire
  | n when n <= 21 -> Hazard Accident
  | n when n <= 35 -> Remedy Drive
  | n when n <= 41 -> Remedy EndOfSpeedLimit
  | n when n <= 47 -> Remedy Gas
  | n when n <= 53 -> Remedy SpareTire
  | n when n <= 59 -> Remedy Repairs
  | n when n <= 69 -> Distance D25
  | n when n <= 79 -> Distance D50
  | n when n <= 89 -> Distance D75
  | n when n <= 101 -> Distance D100
  | _ -> Distance D200

let generate_initial_pile : unit -> pile_of_card =
 fun () -> List.init 106 init_card_from_int
