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
