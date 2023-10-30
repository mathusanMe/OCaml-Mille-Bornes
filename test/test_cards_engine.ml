open Mille_bornes.Cards_engine
open Utils_cards_engine

let test_pp_deck_of_card1 =
  Alcotest.test_case "exemple_pp_deck_of_card1" `Quick (fun () ->
      Alcotest.(check string)
        "same result" "Deck 1 : 0. 200;\n         \n"
        (Format.asprintf "%a" (pp_deck_of_card "Deck 1") exemple_pp_list1))

let test_pp_deck_of_card2 =
  Alcotest.test_case "exemple_pp_deck_of_card2" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Deck 2 : 0. 25;\n\
        \         1. Drive;\n\
        \         2. Accident;\n\
        \         3. Emergency vehicle;\n\
        \         \n"
        (Format.asprintf "%a" (pp_deck_of_card "Deck 2") exemple_pp_list2))

let test_pp_deck_of_card3 =
  Alcotest.test_case "exemple_pp_deck_of_card3" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Deck 3 : 0. Drive;\n\
        \         1. Spare tire;\n\
        \         2. Out of gas;\n\
        \         3. Fuel truck;\n\
        \         4. Plat tire;\n\
        \         5. 100;\n\
        \         6. 25;\n\
        \         7. 100;\n\
        \         8. 100;\n\
        \         9. 200;\n\
        \         10. 200;\n\
        \         \n"
        (Format.asprintf "%a" (pp_deck_of_card "Deck 3") exemple_pp_list3))

let test_pp_pile_of_card1 =
  Alcotest.test_case "exemple_pp_deck_of_card1" `Quick (fun () ->
      Alcotest.(check string)
        "same result" "Pile 1 : 200;\n         \n"
        (Format.asprintf "%a" (pp_pile_of_card "Pile 1") exemple_pp_list1))

let test_pp_pile_of_card2 =
  Alcotest.test_case "exemple_pp_pile_of_card2" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Pile 2 : 25;\n\
        \         Drive;\n\
        \         Accident;\n\
        \         Emergency vehicle;\n\
        \         \n"
        (Format.asprintf "%a" (pp_pile_of_card "Pile 2") exemple_pp_list2))

let test_pp_pile_of_card3 =
  Alcotest.test_case "exemple_pp_pile_of_card3" `Quick (fun () ->
      Alcotest.(check string)
        "same result"
        "Pile 3 : Drive;\n\
        \         Spare tire;\n\
        \         Out of gas;\n\
        \         Fuel truck;\n\
        \         Plat tire;\n\
        \         100;\n\
        \         25;\n\
        \         100;\n\
        \         100;\n\
        \         200;\n\
        \         200;\n\
        \         \n"
        (Format.asprintf "%a" (pp_pile_of_card "Pile 3") exemple_pp_list3))

let () =
  let open Alcotest in
  run "Cards_engine"
    [
      ( "pp_deck_of_card",
        [ test_pp_deck_of_card1; test_pp_deck_of_card2; test_pp_deck_of_card3 ]
      );
      ( "pp_pile_of_card",
        [ test_pp_pile_of_card1; test_pp_pile_of_card2; test_pp_pile_of_card3 ]
      );
    ]
