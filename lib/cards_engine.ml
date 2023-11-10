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
  | n when n >= 4 && n <= 8 -> Hazard Stop
  | n when n >= 9 && n <= 12 -> Hazard SpeedLimit
  | n when n >= 13 && n <= 15 -> Hazard OutOfGas
  | n when n >= 16 && n <= 18 -> Hazard FlatTire
  | n when n >= 19 && n <= 21 -> Hazard Accident
  | n when n >= 22 && n <= 35 -> Remedy Drive
  | n when n >= 36 && n <= 41 -> Remedy EndOfSpeedLimit
  | n when n >= 42 && n <= 47 -> Remedy Gas
  | n when n >= 48 && n <= 53 -> Remedy SpareTire
  | n when n >= 54 && n <= 59 -> Remedy Repairs
  | n when n >= 60 && n <= 69 -> Distance D25
  | n when n >= 70 && n <= 79 -> Distance D50
  | n when n >= 80 && n <= 89 -> Distance D75
  | n when n >= 90 && n <= 101 -> Distance D100
  | n when n >= 102 && n <= 105 -> Distance D200
  | _ -> raise Fail_init_card

let generate_initial_pile : unit -> pile_of_card =
 fun () -> List.init 106 init_card_from_int

exception Empty_pile

let peek_card_from_draw_pile (p : pile_of_card) =
  match p with [] -> raise Empty_pile | h :: _ -> h

let draw_card_from_pile (p : pile_of_card) =
  match p with [] -> raise Empty_pile | h :: t -> (h, t)

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

(* Returns the list with v1 in place of i2 and v2 in place of i1 *)
let swap_value p i1 i2 v1 v2 =
  if i1 = i2 then p
  else
    let rec aux_switch_value i acc lst =
      match lst with
      | [] -> List.rev acc
      | hd :: tl ->
          if i1 = i then aux_switch_value (i + 1) (v2 :: acc) tl
          else if i2 = i then aux_switch_value (i + 1) (v1 :: acc) tl
          else aux_switch_value (i + 1) (hd :: acc) tl
    in
    aux_switch_value 0 [] p

let shuffle_pile p =
  let _ = Random.self_init () in
  let n = List.length p in
  let rec aux_shuffle_pile p_acc i =
    if i = n then p_acc
    else
      let r = Random.int n in
      aux_shuffle_pile
        (swap_value p_acc i r (List.nth p_acc i) (List.nth p_acc r))
        (i + 1)
  in
  aux_shuffle_pile p 0

let get_hazard_corresponding_to_the_remedy (c : remedy_card) =
  match c with
  | Drive -> Stop
  | EndOfSpeedLimit -> SpeedLimit
  | Gas -> OutOfGas
  | SpareTire -> FlatTire
  | Repairs -> Accident

let add_card_to_pile (p : pile_of_card) (c : card) = c :: p
let add_card_to_deck (d : deck_of_card) (c : card) = sort_card_list (c :: d)
