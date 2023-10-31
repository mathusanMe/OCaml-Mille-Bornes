type remedy_card = Drive | EndOfSpeedLimit | Gas | SpareTire | Repairs
[@@deriving eq]

type hazard_card = Stop | SpeedLimit | OutOfGas | FlatTire | Accident
[@@deriving eq]

type safety_card = EmergencyVehicle | FuelTruck | PunctureProof | DrivingAce
[@@deriving eq]

type distance_card = D25 | D50 | D75 | D100 | D200 [@@deriving eq]

type card =
  | Remedy of remedy_card
  | Hazard of hazard_card
  | Safety of safety_card
  | Distance of distance_card
[@@deriving eq]

type deck_of_card = card list [@@deriving eq]
type pile_of_card = card list [@@deriving eq]

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

let pp_remedy fmt r = Format.fprintf fmt "%s" (remedy_to_string r)
let pp_hazard fmt h = Format.fprintf fmt "%s" (hazard_to_string h)
let pp_safety fmt s = Format.fprintf fmt "%s" (safety_to_string s)
let pp_distance fmt d = Format.fprintf fmt "%s" (distance_to_string d)

let pp_card fmt = function
  | Remedy r -> pp_remedy fmt r
  | Hazard h -> pp_hazard fmt h
  | Safety s -> pp_safety fmt s
  | Distance d -> pp_distance fmt d

let pp_list_of_card with_index fmt l =
  if with_index then
    l
    |> List.iteri (fun i e ->
           Format.fprintf fmt "%d. %a" i pp_card e;
           Format.fprintf fmt ";@;")
  else
    l
    |> List.iter (fun e ->
           Format.fprintf fmt "%a" pp_card e;
           Format.fprintf fmt ";@;")

let pp_deck_of_card name fmt l =
  Format.fprintf fmt "%s : " name;
  Format.fprintf fmt "@[<v>%a@]@;" (pp_list_of_card true) l

let pp_pile_of_card name fmt l =
  Format.fprintf fmt "%s : " name;
  Format.fprintf fmt "@[<v>%a@]@;" (pp_list_of_card false) l

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

let card_to_int = function
  | Safety EmergencyVehicle -> 0
  | Safety FuelTruck -> 1
  | Safety PunctureProof -> 2
  | Safety DrivingAce -> 3
  | Hazard Stop -> 4
  | Hazard SpeedLimit -> 5
  | Hazard OutOfGas -> 6
  | Hazard FlatTire -> 7
  | Hazard Accident -> 8
  | Remedy Drive -> 9
  | Remedy EndOfSpeedLimit -> 10
  | Remedy Gas -> 11
  | Remedy SpareTire -> 12
  | Remedy Repairs -> 13
  | Distance D25 -> 14
  | Distance D50 -> 15
  | Distance D75 -> 16
  | Distance D100 -> 17
  | Distance D200 -> 18

(* Compare c1 and c2 using card_to_int *)
let compare_card c1 c2 =
  let n1 = card_to_int c1 in
  let n2 = card_to_int c2 in
  if n1 < n2 then -1 else if n1 > n2 then 1 else 0

let sort_card_list l = List.sort compare_card l

let draw_card_from_pile (p : pile_of_card) =
  match p with [] -> failwith "Empty pile" | h :: t -> (h, t)

let get_hazard_corresponding_to_the_remedy (c : remedy_card) =
  match c with
  | Drive -> Stop
  | EndOfSpeedLimit -> SpeedLimit
  | Gas -> OutOfGas
  | SpareTire -> FlatTire
  | Repairs -> Accident
