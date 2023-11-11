open Mille_bornes.Cards_engine
open Utils_cards_engine

let test_pp_deck_of_card1 =
  Alcotest.test_case "test pp_deck_of_card on exemple_pp_deck_of_card1" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result" "Deck 1 : 0. 200;\n         \n"
        (Format.asprintf "%a" (pp_deck_of_card "Deck 1") exemple_pp_list1))

let test_pp_deck_of_card2 =
  Alcotest.test_case "test pp_deck_of_card on exemple_pp_deck_of_card2" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Deck 2 : 0. 25;\n\
        \         1. Drive;\n\
        \         2. Accident;\n\
        \         3. Emergency vehicle;\n\
        \         \n"
        (Format.asprintf "%a" (pp_deck_of_card "Deck 2") exemple_pp_list2))

let test_pp_deck_of_card3 =
  Alcotest.test_case "test pp_deck_of_card on exemple_pp_deck_of_card3" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Deck 3 : 0. Drive;\n\
        \         1. Spare tire;\n\
        \         2. Out of gas;\n\
        \         3. Fuel truck;\n\
        \         4. Flat tire;\n\
        \         5. 100;\n\
        \         6. 25;\n\
        \         7. 100;\n\
        \         8. 100;\n\
        \         9. 200;\n\
        \         10. 200;\n\
        \         \n"
        (Format.asprintf "%a" (pp_deck_of_card "Deck 3") exemple_pp_list3))

let test_pp_deck_of_card4 =
  Alcotest.test_case "test pp_deck_of_card on an empty deck" `Quick (fun () ->
      Alcotest.(check string)
        "same result" "Empty deck : (empty);\n             \n"
        (Format.asprintf "%a" (pp_deck_of_card "Empty deck") []))

let test_pp_pile_of_card1 =
  Alcotest.test_case "test pp_pile_of_card on exemple_pp_pile_of_card1" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result" "Pile 1 : 200;\n         \n"
        (Format.asprintf "%a" (pp_pile_of_card "Pile 1") exemple_pp_list1))

let test_pp_pile_of_card2 =
  Alcotest.test_case "test pp_pile_of_card on exemple_pp_pile_of_card2" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Pile 2 : 25;\n\
        \         Drive;\n\
        \         Accident;\n\
        \         Emergency vehicle;\n\
        \         \n"
        (Format.asprintf "%a" (pp_pile_of_card "Pile 2") exemple_pp_list2))

let test_pp_pile_of_card3 =
  Alcotest.test_case "test pp_pile_of_card on exemple_pp_pile_of_card3" `Quick
    (fun () ->
      Alcotest.(check string)
        "same result"
        "Pile 3 : Drive;\n\
        \         Spare tire;\n\
        \         Out of gas;\n\
        \         Fuel truck;\n\
        \         Flat tire;\n\
        \         100;\n\
        \         25;\n\
        \         100;\n\
        \         100;\n\
        \         200;\n\
        \         200;\n\
        \         \n"
        (Format.asprintf "%a" (pp_pile_of_card "Pile 3") exemple_pp_list3))

let test_pp_pile_of_card4 =
  Alcotest.test_case "test pp_pile_of_card on an empty pile" `Quick (fun () ->
      Alcotest.(check string)
        "same result" "Empty pile : (empty);\n             \n"
        (Format.asprintf "%a" (pp_pile_of_card "Empty pile") []))

let test_sort_card_list1 =
  Alcotest.test_case "test sort_card_list on exemple_list_to_sort1" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           (sort_card_list exemple_list_to_sort1)
           [ Remedy Drive ]))

let test_sort_card_list2 =
  Alcotest.test_case "test sort_card_list on exemple_list_to_sort2" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           (sort_card_list exemple_list_to_sort2)
           [ Safety DrivingAce; Remedy Gas; Hazard Accident; Distance D200 ]))

let test_sort_card_list3 =
  Alcotest.test_case "test sort_card_list on exemple_list_to_sort3" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           (sort_card_list exemple_list_to_sort3)
           [
             Safety EmergencyVehicle;
             Safety FuelTruck;
             Remedy Drive;
             Remedy Drive;
             Remedy SpareTire;
             Hazard OutOfGas;
             Hazard FlatTire;
             Hazard Accident;
             Distance D25;
             Distance D25;
             Distance D100;
             Distance D100;
             Distance D100;
             Distance D200;
             Distance D200;
             Distance D200;
           ]))

let test_sort_card_list4 =
  Alcotest.test_case "test sort_card_list on exemple_list_to_sort4" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           (sort_card_list exemple_list_to_sort4)
           [
             Safety EmergencyVehicle;
             Safety FuelTruck;
             Remedy Drive;
             Remedy Drive;
             Remedy SpareTire;
             Hazard OutOfGas;
             Hazard FlatTire;
             Hazard Accident;
             Distance D25;
             Distance D25;
             Distance D100;
             Distance D100;
             Distance D100;
             Distance D200;
             Distance D200;
             Distance D200;
           ]))

let test_shuffle_pile =
  let g = generate_initial_pile () in
  Alcotest.test_case
    "test shuffle_pile on the pile generated by the function \
     generate_initial_pile"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_pile_of_card g (sort_card_list (shuffle_pile g))))

let test_get_hazard_corresponding_to_the_remedy =
  Alcotest.test_case "test get_hazard_corresponding_to_the_remedy" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_hazard_corresponding_to_the_remedy Drive = Stop
        && get_hazard_corresponding_to_the_remedy EndOfSpeedLimit = SpeedLimit
        && get_hazard_corresponding_to_the_remedy Gas = OutOfGas
        && get_hazard_corresponding_to_the_remedy SpareTire = FlatTire
        && get_hazard_corresponding_to_the_remedy Repairs = Accident))

let test_add_card_to_pile1 =
  Alcotest.test_case "test add_card_to_pile on empty pile" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card [ Hazard Stop ]
           (add_card_to_pile exemple_pile1 (Hazard Stop))))

let test_add_card_to_pile2 =
  Alcotest.test_case "test add_card_to_pile on non-empty pile" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           [ Remedy EndOfSpeedLimit; Remedy Drive ]
           (add_card_to_pile exemple_pile2 (Remedy EndOfSpeedLimit))))

let test_add_card_to_deck1 =
  Alcotest.test_case "test add_card_to_deck on empty deck" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           [ Safety EmergencyVehicle ]
           (add_card_to_deck exemple_deck1 (Safety EmergencyVehicle))))

let test_add_card_to_deck2 =
  Alcotest.test_case "test add_card_to_deck on non-empty deck" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           [ Remedy Drive; Remedy EndOfSpeedLimit ]
           (add_card_to_deck exemple_deck2 (Remedy EndOfSpeedLimit))))

let test_add_card_to_deck3 =
  Alcotest.test_case "test add_card_to_deck on in a deck with many cards" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (equal_deck_of_card
           [
             Safety FuelTruck;
             Remedy Drive;
             Remedy EndOfSpeedLimit;
             Remedy SpareTire;
             Hazard OutOfGas;
             Hazard FlatTire;
             Distance D25;
             Distance D100;
             Distance D100;
             Distance D100;
             Distance D200;
             Distance D200;
           ]
           (add_card_to_deck exemple_deck3 (Remedy EndOfSpeedLimit))))

let () =
  let open Alcotest in
  run "Cards_engine"
    [
      ( "pp_deck_of_card",
        [
          test_pp_deck_of_card1;
          test_pp_deck_of_card2;
          test_pp_deck_of_card3;
          test_pp_deck_of_card4;
        ] );
      ( "pp_pile_of_card",
        [
          test_pp_pile_of_card1;
          test_pp_pile_of_card2;
          test_pp_pile_of_card3;
          test_pp_pile_of_card4;
        ] );
      ( "sort_card_list",
        [
          test_sort_card_list1;
          test_sort_card_list2;
          test_sort_card_list3;
          test_sort_card_list4;
        ] );
      ("shuffle_pile", [ test_shuffle_pile ]);
      ( "test get_hazard_corresponding_to_the_remedy",
        [ test_get_hazard_corresponding_to_the_remedy ] );
      ( "add_card to_deck and to_pile",
        [
          test_add_card_to_pile1;
          test_add_card_to_pile2;
          test_add_card_to_deck1;
          test_add_card_to_deck2;
          test_add_card_to_deck3;
        ] );
    ]