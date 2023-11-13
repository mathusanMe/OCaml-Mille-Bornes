type safety_card = EmergencyVehicle | FuelTruck | PunctureProof | DrivingAce
[@@deriving eq]

type hazard_card = Stop | SpeedLimit | OutOfGas | FlatTire | Accident
[@@deriving eq]

type remedy_card = Drive | EndOfSpeedLimit | Gas | SpareTire | Repairs
[@@deriving eq]

type distance_card = D25 | D50 | D75 | D100 | D200 [@@deriving eq]

type card =
  | Safety of safety_card
  | Remedy of remedy_card
  | Hazard of hazard_card
  | Distance of distance_card
[@@deriving eq]

type deck_of_card = card list [@@deriving eq]
type pile_of_card = card list [@@deriving eq]

let is_empty (l : card list) = match l with [] -> true | _ -> false

let pp_remedy fmt r =
  let r_str =
    match r with
    | Drive -> "Drive"
    | EndOfSpeedLimit -> "End of speed limit"
    | Gas -> "Gas"
    | SpareTire -> "Spare tire"
    | Repairs -> "Repairs"
  in
  Format.fprintf fmt "%s" r_str

let pp_hazard fmt h =
  let h_str =
    match h with
    | Stop -> "Stop"
    | SpeedLimit -> "Speed limit"
    | OutOfGas -> "Out of gas"
    | FlatTire -> "Flat tire"
    | Accident -> "Accident"
  in
  Format.fprintf fmt "%s" h_str

let pp_safety fmt s =
  let s_str =
    match s with
    | EmergencyVehicle -> "Emergency vehicle"
    | FuelTruck -> "Fuel truck"
    | PunctureProof -> "Puncture proof"
    | DrivingAce -> "Driving ace"
  in
  Format.fprintf fmt "%s" s_str

let pp_distance fmt d =
  let d_str =
    match d with
    | D25 -> "25"
    | D50 -> "50"
    | D75 -> "75"
    | D100 -> "100"
    | D200 -> "200"
  in
  Format.fprintf fmt "%s" d_str

let pp_card fmt = function
  | Remedy r -> pp_remedy fmt r
  | Hazard h -> pp_hazard fmt h
  | Safety s -> pp_safety fmt s
  | Distance d -> pp_distance fmt d

let pp_list_of_card with_index fmt l =
  if is_empty l then Format.fprintf fmt "(empty);@;"
  else if with_index then
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

let pp_top_pile_of_card name fmt l =
  Format.fprintf fmt "%s : " name;
  if is_empty l then Format.fprintf fmt "(empty);@;"
  else Format.fprintf fmt "%a;@;" pp_card (List.hd l)

exception Fail_init_card

let init_card_from_int = function
  | 0 -> Safety EmergencyVehicle
  | 1 -> Safety FuelTruck
  | 2 -> Safety PunctureProof
  | 3 -> Safety DrivingAce
  | n when n >= 4 && n <= 17 -> Remedy Drive
  | n when n >= 18 && n <= 23 -> Remedy EndOfSpeedLimit
  | n when n >= 24 && n <= 29 -> Remedy Gas
  | n when n >= 30 && n <= 35 -> Remedy SpareTire
  | n when n >= 36 && n <= 41 -> Remedy Repairs
  | n when n >= 42 && n <= 46 -> Hazard Stop
  | n when n >= 47 && n <= 50 -> Hazard SpeedLimit
  | n when n >= 51 && n <= 53 -> Hazard OutOfGas
  | n when n >= 54 && n <= 56 -> Hazard FlatTire
  | n when n >= 57 && n <= 59 -> Hazard Accident
  | n when n >= 60 && n <= 69 -> Distance D25
  | n when n >= 70 && n <= 79 -> Distance D50
  | n when n >= 80 && n <= 89 -> Distance D75
  | n when n >= 90 && n <= 101 -> Distance D100
  | n when n >= 102 && n <= 105 -> Distance D200
  | _ -> raise Fail_init_card

let generate_initial_pile : unit -> pile_of_card =
 fun () -> List.init 106 init_card_from_int

exception Empty_pile
exception Empty_deck

let peek_card_from_pile (p : pile_of_card) =
  match p with [] -> raise Empty_pile | h :: _ -> h

let draw_card_from_pile (p : pile_of_card) =
  match p with [] -> raise Empty_pile | h :: t -> (h, t)

let sort_card_list l = List.sort compare l

let shuffle_pile (p : pile_of_card) =
  let _ = Random.self_init () in
  let rec aux_shuffle_pile = function
    | [] -> []
    | [ card ] -> [ card ]
    | p ->
        let left, right = List.partition (fun _ -> Random.bool ()) p in
        List.rev_append (aux_shuffle_pile left) (aux_shuffle_pile right)
  in
  aux_shuffle_pile p

let get_hazard_corresponding_to_the_remedy (c : remedy_card) =
  match c with
  | Drive -> Stop
  | EndOfSpeedLimit -> SpeedLimit
  | Gas -> OutOfGas
  | SpareTire -> FlatTire
  | Repairs -> Accident

let get_remedy_corresponding_to_the_hazard (c : hazard_card) =
  match c with
  | Stop -> Drive
  | SpeedLimit -> EndOfSpeedLimit
  | OutOfGas -> Gas
  | FlatTire -> SpareTire
  | Accident -> Repairs

let get_safety_corresponding_to_the_hazard (c : hazard_card) =
  match c with
  | Stop | SpeedLimit -> EmergencyVehicle
  | OutOfGas -> FuelTruck
  | FlatTire -> PunctureProof
  | Accident -> DrivingAce

let add_card_to_pile (p : pile_of_card) (c : card) = c :: p
let add_card_to_deck (d : deck_of_card) (c : card) = sort_card_list (c :: d)

let rec remove_card_from_deck (d : deck_of_card) (c : card) =
  match d with
  | [] -> raise Empty_deck
  | h :: t -> if h = c then t else h :: remove_card_from_deck t c
