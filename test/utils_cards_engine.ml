open Mille_bornes.Cards_engine

let exemple_pp_list1 = [ Distance D200 ]

let exemple_pp_list2 =
  [ Distance D25; Remedy Drive; Hazard Accident; Safety EmergencyVehicle ]

let exemple_pp_list3 =
  [
    Remedy Drive;
    Remedy SpareTire;
    Hazard OutOfGas;
    Safety FuelTruck;
    Hazard FlatTire;
    Distance D100;
    Distance D25;
    Distance D100;
    Distance D100;
    Distance D200;
    Distance D200;
  ]

let exemple_list_to_sort1 = [ Remedy Drive ]

let exemple_list_to_sort2 =
  [ Distance D200; Remedy Gas; Hazard Accident; Safety DrivingAce ]

let exemple_list_to_sort3 =
  exemple_pp_list1 @ exemple_pp_list2 @ exemple_pp_list3

let exemple_list_to_sort4 = List.rev exemple_list_to_sort3
let exemple_pile1 = []
let exemple_pile2 = [ Remedy Drive ]
let exemple_deck1 = []
let exemple_deck2 = [ Remedy Drive ]

let exemple_deck3 =
  [
    Remedy Drive;
    Remedy SpareTire;
    Hazard OutOfGas;
    Safety FuelTruck;
    Hazard FlatTire;
    Distance D100;
    Distance D25;
    Distance D100;
    Distance D100;
    Distance D200;
    Distance D200;
  ]

let generator_remedy_card =
  let open QCheck in
  Gen.oneof
    [
      Gen.return Drive;
      Gen.return EndOfSpeedLimit;
      Gen.return Gas;
      Gen.return SpareTire;
      Gen.return Repairs;
    ]

let generator_hazard_card =
  let open QCheck in
  Gen.oneof
    [
      Gen.return Stop;
      Gen.return SpeedLimit;
      Gen.return OutOfGas;
      Gen.return FlatTire;
      Gen.return Accident;
    ]

let generator_safety_card =
  let open QCheck in
  Gen.oneof
    [
      Gen.return EmergencyVehicle;
      Gen.return FuelTruck;
      Gen.return PunctureProof;
      Gen.return DrivingAce;
    ]

let generator_distance_card =
  let open QCheck in
  Gen.oneof
    [
      Gen.return D25;
      Gen.return D50;
      Gen.return D75;
      Gen.return D100;
      Gen.return D200;
    ]

let generator_card =
  let open QCheck in
  Gen.oneof
    [
      Gen.map (fun s -> Safety s) generator_safety_card;
      Gen.map (fun r -> Remedy r) generator_remedy_card;
      Gen.map (fun h -> Hazard h) generator_hazard_card;
      Gen.map (fun d -> Distance d) generator_distance_card;
    ]

let arbitrary_card =
  QCheck.make ~print:(Format.asprintf "%a" pp_card) generator_card

let arbitrary_init_pile =
  let open QCheck in
  make
    ~print:(Format.asprintf "%a" (pp_pile_of_card "Pile"))
    (generate_initial_pile () |> Gen.shuffle_l)
