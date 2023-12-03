open Mille_bornes.Cards_engine
open Utils_cards_engine

let test_is_empty_true =
  Alcotest.test_case "test if is_empty returns true with an empty list" `Quick
    (fun () -> Alcotest.(check bool) "same result" (is_empty []) true)

let test_is_empty_false =
  Alcotest.test_case "test if is_empty returns false with a non empty list"
    `Quick (fun () ->
      Alcotest.(check bool) "same result" (is_empty not_empty_card_list) false)

let test_equal_card_true =
  Alcotest.test_case "test if equal_card returns true with the same cards"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result"
        (equal_card (Hazard Accident) (Hazard Accident))
        true)

let test_equal_card_false1 =
  Alcotest.test_case
    "test if equal_card returns false with two diffrent cards of the same type"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result"
        (equal_card (Hazard Accident) (Hazard OutOfGas))
        false)

let test_equal_card_false2 =
  Alcotest.test_case
    "test if equal_card returns false with two diffrent cards of diffrent types"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result"
        (equal_card (Hazard Accident) (Distance D75))
        false)

let test_equal_deck_true =
  Alcotest.test_case
    "test if equal_deck_of_cards returns true with two same decks" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result"
        (equal_deck_of_card not_empty_card_list not_empty_card_list)
        true)

let test_equal_deck_false1 =
  Alcotest.test_case
    "test if equal_deck_of_cards returns false with a non empty and an empty \
     card lists"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result"
        (equal_deck_of_card not_empty_card_list [])
        false)

let test_equal_deck_false2 =
  Alcotest.test_case
    "test if equal_deck_of_cards returns false with two diffrent card lists \
     having the same elements in different order"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result"
        (equal_deck_of_card not_empty_card_list deck_different_order)
        false)

let test_equal_deck_false3 =
  Alcotest.test_case
    "test if equal_deck_of_cards returns false with two diffrent card lists"
    `Quick (fun () ->
      Alcotest.(check bool)
        "same result"
        (equal_deck_of_card not_empty_card_list deck_different_cards)
        false)

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

let test_get_remedy_corresponding_to_the_hazard =
  Alcotest.test_case "test get_remedy_corresponding_to_the_hazard" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_remedy_corresponding_to_the_hazard Stop = Drive
        && get_remedy_corresponding_to_the_hazard SpeedLimit = EndOfSpeedLimit
        && get_remedy_corresponding_to_the_hazard OutOfGas = Gas
        && get_remedy_corresponding_to_the_hazard FlatTire = SpareTire
        && get_remedy_corresponding_to_the_hazard Accident = Repairs))

let test_get_safety_corresponding_to_the_hazard =
  Alcotest.test_case "test get_safety_corresponding_to_the_hazard" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (get_safety_corresponding_to_the_hazard Stop = EmergencyVehicle
        && get_safety_corresponding_to_the_hazard SpeedLimit = EmergencyVehicle
        && get_safety_corresponding_to_the_hazard OutOfGas = FuelTruck
        && get_safety_corresponding_to_the_hazard FlatTire = PunctureProof
        && get_safety_corresponding_to_the_hazard Accident = DrivingAce))

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

let test_remove_mem_card_from_deck =
  let open QCheck in
  Test.make ~count:1000
    ~name:
      "For all card present in deck (when 1 <= length of deck <= 1000), \
       (remove_card_from_deck deck card) = deck - {card}"
    (list_of_size (Gen.int_range 1 1000) arbitrary_card)
    (fun pile ->
      let to_remove = List.length pile |> Random.int |> List.nth pile in
      let different_to_remove_list =
        List.filter (fun card -> not (equal_card card to_remove)) pile
      in
      let same_to_remove_list =
        List.filter (fun card -> equal_card card to_remove) pile
      in
      let nb_left = max 0 ((same_to_remove_list |> List.length) - 1) in
      let expected_deck =
        different_to_remove_list @ List.init nb_left (fun _ -> to_remove)
      in
      equal_deck_of_card
        (remove_card_from_deck pile to_remove |> sort_card_list)
        (expected_deck |> sort_card_list))

let test_remove_non_mem_card_from_deck =
  let deck =
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
    ]
  in
  let to_remove = Distance D200 in
  Alcotest.test_case "test remove_card_from_deck when card is not in deck"
    `Quick (fun () ->
      Alcotest.check_raises "Expected Card_not_found" Card_not_found (fun () ->
          ignore (remove_card_from_deck deck to_remove)))

let test_is_empty_on_non_empty =
  let open QCheck in
  Test.make ~count:1000
    ~name:"Forall non-empty card_list, is_empty card_list = false"
    (list arbitrary_card) (fun card_list ->
      assume (card_list <> []);
      not (is_empty card_list))

let test_is_empty_on_empty =
  let open Alcotest in
  test_case "is_empty [] = true" `Quick (fun () ->
      (check bool) "same" true (is_empty []))

let () =
  Random.self_init ();
  let open Alcotest in
  run "Cards_engine"
    [
      ("is_empty", [ test_is_empty_true; test_is_empty_false ]);
      ( "equal_card",
        [ test_equal_card_true; test_equal_card_false1; test_equal_card_false2 ]
      );
      ( "equal_deck_of_cards",
        [
          test_equal_deck_true;
          test_equal_deck_false1;
          test_equal_deck_false2;
          test_equal_deck_false3;
        ] );
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
      ( "test get_remedy_corresponding_to_the_hazard",
        [ test_get_remedy_corresponding_to_the_hazard ] );
      ( "test get_safety_corresponding_to_the_hazard",
        [ test_get_safety_corresponding_to_the_hazard ] );
      ( "add_card to_deck and to_pile",
        [
          test_add_card_to_pile1;
          test_add_card_to_pile2;
          test_add_card_to_deck1;
          test_add_card_to_deck2;
          test_add_card_to_deck3;
        ] );
      ( "test_remove_mem_card_from_deck",
        [ QCheck_alcotest.to_alcotest test_remove_mem_card_from_deck ] );
      ( "test_remove_non_mem_card_from_deck",
        [ test_remove_non_mem_card_from_deck ] );
      ( "test is_empty",
        [
          QCheck_alcotest.to_alcotest test_is_empty_on_non_empty;
          test_is_empty_on_empty;
        ] );
    ]
