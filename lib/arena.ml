open Teams_engine
open Board_engine
open Cards_engine
open Random_bot

type endplay =
  | Equality
  | Win of string list
  | GiveUpInGame of string list
  | GiveUpInit
  | Error of string

let pp_endplay fmt result =
  match result with
  | Equality ->
      Format.printf
        "There are no more cards and no one has reached 1000 miles. You have \
         all lost!@ "
  | Win names ->
      Format.fprintf fmt "Winner(s) : %a@ "
        (fun fmt lst -> List.iter (fun e -> Format.fprintf fmt "%s@ " e) lst)
        names
  | GiveUpInGame names ->
      Format.fprintf fmt "Give up of : %a@ "
        (fun fmt lst -> List.iter (fun e -> Format.fprintf fmt "%s@ " e) lst)
        names
  | GiveUpInit ->
      Format.fprintf fmt "Give up during initialization of the game@ "
  | Error message -> Format.fprintf fmt "%s" message

let has_win t = t |> get_public_informations_from |> get_score_from >= 1000
let get_strategy_list () = [ random_strategy ]

let pp_strategy_list fmt strategy_list =
  List.iteri (fun i s -> Format.fprintf fmt "%d.%s@ " i s.name) strategy_list

let rec request_number request =
  Format.printf "%s (write `exit` to quit):@ " request;
  Format.print_flush ();
  try
    Scanf.scanf "%s@\n" (fun s ->
        if s = "exit" then (
          Format.printf "You've given up. Come and play again another day!@;";
          None)
        else Some (int_of_string s))
  with Failure _ ->
    Format.printf "Invalid entry : a number is requested.@;";
    request_number request

let rec request_id request max =
  match request_number request with
  | None -> None
  | Some i ->
      if i > max || i < 0 then (
        Format.printf "Invalid entry : the number is not correct.@;";
        request_id request max)
      else Some i

let rec request_yes_or_no request =
  Format.printf "%s (y or n to answer or write `exit` to quit):@ " request;
  Format.print_flush ();
  Scanf.scanf "%s@\n" (fun s ->
      if s = "exit" then (
        Format.printf "You've given up. Come and play again another day!@;";
        None)
      else if s = "y" then Some true
      else if s = "n" then Some false
      else (
        Format.printf "Invalid entry : your answer is not correct.@;";
        request_yes_or_no request))

let rec request_answer_with_length request max_length =
  Format.printf "%s (no longer than %d or write `exit` to quit):@ " request
    max_length;
  Format.print_flush ();
  Scanf.scanf "%s@\n" (fun s ->
      if s = "exit" then (
        Format.printf "You've given up. Come and play again another day!@;";
        None)
      else
        let new_s = String.trim s in
        let new_s_length = String.length new_s in
        if new_s_length > max_length then (
          Format.printf
            "Invalid entry : your answer (with start and end space removed) is \
             too long.@;";
          request_answer_with_length request max_length)
        else if new_s_length = 0 then (
          Format.printf
            "Invalid entry : empty or spaces only answers are not accepted.@;";
          request_answer_with_length request max_length)
        else Some new_s)

let rec request_number_of_player () =
  match
    request_number
      "Enter the number of players (between 2, 3 and 4 for solo games, and \
       between 4, 6 and 8 for team games)"
  with
  | None -> None
  | Some i ->
      if i != 2 && i != 3 && i != 4 && i != 6 && i != 8 then (
        Format.printf "Invalid entry : the number is not correct.@;";
        request_number_of_player ())
      else if i = 4 then
        match
          request_yes_or_no "Do you want to play in team of two players ?"
        with
        | None -> None
        | Some is_team_of_two -> Some (i, is_team_of_two)
      else if i < 4 then Some (i, false)
      else Some (i, true)

let request_strategy_for_bot id num =
  let strategy_list = get_strategy_list () in
  let strategy_list_length = List.length strategy_list in
  if strategy_list_length = 0 then (
    Format.printf
      "There is no existant strategy, please add it into the game and come \
       back again !";
    None)
  else if strategy_list_length = 1 then (
    let strategy = List.hd strategy_list in
    Format.printf
      "As it is alone, the strategy chosen for the bot %d of the team %d is %s@;"
      num id strategy.name;
    Some strategy)
  else (
    Format.printf "%a@ " pp_strategy_list strategy_list;
    match
      request_id
        (Format.asprintf
           "With which strategy the bot %d of team %d will play considering \
            the list above ?"
           num id)
        (List.length strategy_list)
    with
    | None -> None
    | Some i -> Some (List.nth strategy_list i))

let player_teletype_choose_card_to_play p pi pi_list =
  let p_hand = get_hand_from p in
  Format.printf
    "%s of team %d, your score is %d, your hand and your driving zone are :@;\
     %a@ @[<v>%a@]@;"
    (get_name_from p) (get_id_from pi) (get_score_from pi)
    (pp_deck_of_card "Hand") p_hand
    (pp_public_informations false)
    pi;
  match
    request_id
      "Choose the id of the card you want to play, or discard, in your hand \
       (the number in front of it)"
      (List.length p_hand - 1)
  with
  | None -> None
  | Some id_card -> (
      match request_yes_or_no "Do you want to discard it ?" with
      | None -> None
      | Some true -> Some (id_card, None)
      | Some false -> (
          match List.nth p_hand id_card with
          | Hazard _ -> (
              Format.printf "Here are all the existing zones : @ %a"
                pp_public_informations_list pi_list;
              match
                request_id "Choose the id of the one you want to attack."
                  (List.length pi_list - 1)
              with
              | None -> None
              | Some id_pi ->
                  Some (id_card, Some (get_id_from (List.nth pi_list id_pi))))
          | _ -> Some (id_card, Some (get_id_from pi))))

let player_teletype_want_to_peek_discard_pile p c pi _ =
  Format.printf
    "%s of team %d, your score is %d, your hand and your driving zone are :@;\
     %a@ @[<v>%a@]@;"
    (get_name_from p) (get_id_from pi) (get_score_from pi)
    (pp_deck_of_card "Hand") (get_hand_from p)
    (pp_public_informations false)
    pi;
  request_yes_or_no
    (Format.asprintf
       "Do you want to pick the card %a of the discard pile ? If not you'll \
        take a random card in draw pile "
       pp_card c)

let player_teletype_want_to_play_coup_fourre p h pi _ =
  Format.printf
    "%s of team %d, your team is attacked by the card %a, your score is %d, \
     your hand and your driving zone are :@;\
     %a\n\
    \      @ @[<v>%a@]@;"
    (get_name_from p) (get_id_from pi) pp_card (Hazard h) (get_score_from pi)
    (pp_deck_of_card "Hand") (get_hand_from p)
    (pp_public_informations false)
    pi;
  request_yes_or_no
    "Do you want to play your safety and do a coup fourre to earn 200 points ?"

let human_strat =
  {
    name = "human strategy";
    choose_card_to_play = player_teletype_choose_card_to_play;
    want_to_peek_discard_pile = player_teletype_want_to_peek_discard_pile;
    want_to_play_coup_fourre = player_teletype_want_to_play_coup_fourre;
  }

let rec ask_player_info id num already_created_team name_first_player =
  let max_name_length = 15 in
  match
    request_answer_with_length
      (Format.asprintf "Give the name of the player %d of the team %d" num id)
      max_name_length
  with
  | None -> None
  | Some name -> (
      if
        List.exists
          (fun (name_to_verify, _) -> name = name_to_verify)
          already_created_team
        ||
        match name_first_player with
        | None -> false
        | Some name1 -> name1 = name
      then (
        Format.printf
          "This name is already taken by someone else, take another.@;";
        ask_player_info id num already_created_team name_first_player)
      else
        match
          request_yes_or_no
            (Format.asprintf "Is the player %d of team %d a bot ?" num id)
        with
        | None -> None
        | Some false -> Some (name, None)
        | Some true -> (
            match request_strategy_for_bot id num with
            | None -> None
            | Some strategy -> Some (name, Some strategy)))

let ask_team_info id is_team_of_two already_created_team =
  match ask_player_info id 1 already_created_team None with
  | None -> None
  | Some (name1, None) -> (
      if not is_team_of_two then Some (Some (name1, human_strat), None)
      else
        match ask_player_info id 2 already_created_team (Some name1) with
        | None -> None
        | Some (name2, None) ->
            Some (Some (name1, human_strat), Some (name2, human_strat))
        | Some (name2, Some strategy) ->
            Some (Some (name1, human_strat), Some (name2, strategy)))
  | Some (name1, Some strategy1) -> (
      if not is_team_of_two then Some (Some (name1, strategy1), None)
      else
        match ask_player_info id 2 already_created_team (Some name1) with
        | None -> None
        | Some (name2, None) ->
            Some (Some (name1, strategy1), Some (name2, human_strat))
        | Some (name2, Some strategy2) ->
            Some (Some (name1, strategy1), Some (name2, strategy2)))

let ask_teams_info () =
  match request_number_of_player () with
  | None -> None
  | Some (nb_player, is_team_of_two) -> (
      let rec aux_ask_teams_info acc_t_info acc_id =
        if acc_id >= nb_player then Some (List.rev acc_t_info)
        else
          let team_info =
            if is_team_of_two then
              ask_team_info (acc_id / 2) is_team_of_two acc_t_info
            else ask_team_info acc_id is_team_of_two acc_t_info
          in
          match team_info with
          | None -> None
          | Some (None, _) -> None
          | Some (Some t_info1, None) ->
              if is_team_of_two then None
              else aux_ask_teams_info (t_info1 :: acc_t_info) (acc_id + 1)
          | Some (Some t_info1, Some t_info2) ->
              if not is_team_of_two then None
              else
                aux_ask_teams_info
                  (t_info2 :: t_info1 :: acc_t_info)
                  (acc_id + 2)
      in
      match aux_ask_teams_info [] 0 with
      | None -> None
      | Some team_infos_list ->
          Format.printf "Teams created : %a@;"
            (fun fmt infos ->
              List.iteri
                (fun i (name, _) ->
                  if is_team_of_two && i mod 2 = 1 then
                    Format.fprintf fmt "%s;@ " name
                  else if is_team_of_two then
                    Format.fprintf fmt "Team %d : %s;" (i / 2) name
                  else Format.fprintf fmt "Team %d : %s;@ " i name)
                infos)
            team_infos_list;
          Some (team_infos_list, is_team_of_two))

let init_board teams_of_board =
  match
    request_id
      "Which of the id teams has the youngest player (or which team should \
       start)?"
      (List.length teams_of_board - 1)
  with
  | None -> None
  | Some id ->
      let board =
        {
          draw_pile = generate_initial_pile () |> shuffle_pile;
          discard_pile = [];
          teams = teams_of_board;
          current_team_index = id;
        }
      in
      Some (draw_initial_hand_to_teams board)

let init_game () =
  let teams_info = ask_teams_info () in
  match teams_info with
  | None -> None
  | Some (infos, is_team_of_two) ->
      let teams = init_teams infos is_team_of_two in
      init_board teams

let get_list_of_other_public_information_than (p_info : public_informations)
    (b : board) =
  List.rev
    (List.fold_left
       (fun acc t ->
         let t_p_info = get_public_informations_from t in
         if p_info = t_p_info then acc else t_p_info :: acc)
       [] b.teams)

exception Invalid_play
exception Invalid_id_public_information
(*the player played a prohibited move*)

let play_move (current_player : player) (current_team : team) (b : board) =
  try
    (get_strat_from current_player).choose_card_to_play current_player
      (get_public_informations_from current_team)
      (get_list_of_other_public_information_than
         (get_public_informations_from current_team)
         b)
  with _ -> raise Invalid_play

exception Discard_card_error
exception Place_card_error
(*The game is broken*)

let try_get_team_corresponding_public_information (id : int) (l : team list) =
  try
    List.find (fun t -> t |> get_public_informations_from |> get_id_from = id) l
  with Not_found ->
    let () = Format.printf "Target public information is not valid.@ " in
    raise Invalid_id_public_information

let try_discard_card (b : board) (current_team : team) (card_used : card) =
  let new_board =
    try discard_card b current_team card_used
    with Team_not_found | Empty_deck | Card_not_found ->
      let () =
        Format.printf "An error occurs when try you to discard the card.@ "
      in
      raise Discard_card_error
  in
  let () =
    Format.printf "The player %s has discard the card %a.@ "
      (get_name_from (get_current_player_from current_team))
      pp_card card_used
  in
  Some new_board

let try_use_coup_fouree (previous_board_befor_place_hazard_card : board)
    (new_board_after_place_hazard_card : board) (current_team : team)
    (card_used : card) (hazard : hazard_card) (target_team : team)
    (id_of_target_public_informations : int) =
  let player_have_counter =
    try
      Some
        (List.find
           (fun p -> has_safety_to_counter_hazard_on_his_hand p hazard)
           (get_players_from target_team))
    with Not_found -> None
  in
  match player_have_counter with
  | None -> Some new_board_after_place_hazard_card
  | Some player_have_counter -> (
      let necessary_safety = get_safety_corresponding_to_the_hazard hazard in
      match
        try
          (get_strat_from player_have_counter).want_to_play_coup_fourre
            player_have_counter hazard
            (get_public_informations_from target_team)
            (get_list_of_other_public_information_than
               (get_public_informations_from target_team)
               previous_board_befor_place_hazard_card)
        with _ -> raise Invalid_play
      with
      | Some true ->
          let new_board =
            try
              place_coup_fouree previous_board_befor_place_hazard_card
                target_team player_have_counter necessary_safety
            with
            | Team_not_found | Player_not_found | Card_not_found | Unusable_card
            | Empty_deck
            ->
              let () =
                Format.printf
                  "An error occurs when try you to place_coup_fouree.@ "
              in
              raise Place_card_error
          in
          let new_board =
            try
              discard_card_from_player new_board current_team
                (get_current_player_from current_team)
                card_used
            with
            | Team_not_found | Player_not_found | Empty_deck | Card_not_found ->
              let () =
                Format.printf
                  "An error occurs when try you to discard_card_from_player \
                   after the coup fouree.@ "
              in
              raise Place_card_error
          in
          let () =
            Format.printf
              "The player %s on team %d has used %a on coup fouree. The team \
               %d earn 200 points and the card %a go on discard pile.@ "
              (get_name_from player_have_counter)
              id_of_target_public_informations pp_card (Safety necessary_safety)
              id_of_target_public_informations pp_card card_used
          in
          Some new_board
      | Some false ->
          let () =
            Format.printf
              "The player %s on team %d has decide to not use %a on coup \
               fouree.@ "
              (get_name_from player_have_counter)
              id_of_target_public_informations pp_card (Safety necessary_safety)
          in
          Some new_board_after_place_hazard_card
      | None -> None)

let try_place_card (b : board) (current_team : team) (current_player : player)
    (card_used : card) (id_of_target_public_informations : int) =
  let target_team =
    try_get_team_corresponding_public_information
      id_of_target_public_informations b.teams
  in
  let new_board =
    try place_card b current_team card_used target_team
    with Team_not_found | Empty_deck | Card_not_found ->
      let () =
        Format.printf "An error occurs when try you to place the card.@ "
      in
      raise Place_card_error
  in
  let () =
    Format.printf "The player %s has use a card %a on team %d@ "
      (get_name_from current_player)
      pp_card card_used id_of_target_public_informations;
    match card_used with
    | Distance _ ->
        let new_current_team = get_current_team_from new_board in
        Format.printf "and the team %d is now at %d distance.@ "
          (new_current_team |> get_public_informations_from |> get_id_from)
          (new_current_team |> get_public_informations_from |> get_id_from)
    | _ -> ()
  in
  match card_used with
  | Hazard hazard ->
      try_use_coup_fouree b new_board current_team card_used hazard target_team
        id_of_target_public_informations
  | Safety _ ->
      let () =
        Format.printf "As you played a safety card, you can play again!@ "
      in
      let new_board =
        try set_previous_current_team_from new_board current_team
        with Team_not_found -> raise Place_card_error
      in
      Some new_board
  | _ -> Some new_board

let rec play_move_player (number_of_try : int) b =
  let retry_play_move_player (b : board) (current_player : player) =
    let () =
      Format.printf "Player %s tried an invalid move@ "
        (get_name_from current_player)
    in
    if number_of_try > 0 then play_move_player (number_of_try - 1) b
    else raise Invalid_play
  in
  let current_team = get_current_team_from b in
  let current_player = get_current_player_from current_team in
  let move = play_move current_player current_team b in
  match move with
  | None -> None (*give up case*)
  | Some (id_card_on_deck_of_current_player, id_of_target_public_informations)
    -> (
      match id_card_on_deck_of_current_player with
      | id when 0 <= id && id < List.length (get_hand_from current_player) -> (
          let card_used =
            nth_hand_player current_player id_card_on_deck_of_current_player
          in
          match id_of_target_public_informations with
          | None -> (
              (*disard card case*)
              match try_discard_card b current_team card_used with
              | None -> None
              | Some b ->
                  Some
                    (switch_current_team_from
                       (switch_current_player_of_current_team_from b)))
          | Some id_of_target_public_informations -> (
              (*place card case*)
              try
                match
                  try_place_card b current_team current_player card_used
                    id_of_target_public_informations
                with
                | None -> None
                | Some b ->
                    Some
                      (switch_current_team_from
                         (switch_current_player_of_current_team_from b))
              with
              | Invalid_id_public_information ->
                  Format.printf "Invalid_id_public_information@ ";
                  retry_play_move_player b current_player
              | Invalid_move ->
                  Format.printf "Invalid_move@ ";
                  retry_play_move_player b current_player
              | Unusable_card ->
                  Format.printf "Unusable_card@ ";
                  retry_play_move_player b current_player))
      | _ -> retry_play_move_player b current_player)

let get_result_to_peek_inside_discard_pile board =
  if is_empty board.discard_pile then (
    Format.printf
      "The discard pile is empty, so the player draw from the draw pile.@ ";
    Some false)
  else
    let last_discard_card = peek_card_from_pile board.discard_pile in
    let current_team = get_current_team_from board in
    let current_player = get_current_player_from current_team in
    let other_public_informations =
      get_list_of_other_public_information_than
        (get_public_informations_from current_team)
        board
    in

    try
      (get_strat_from current_player).want_to_peek_discard_pile current_player
        last_discard_card
        (get_public_informations_from current_team)
        other_public_informations
    with _ -> raise Invalid_play

let arena () =
  match init_game () with
  | None -> GiveUpInit
  | Some board -> (
      let rec round board =
        let current_team = get_current_team_from board in
        let board =
          if is_empty board.draw_pile then
            swap_draw_and_shuffled_discard_pile board
          else board
        in
        match get_result_to_peek_inside_discard_pile board with
        | None -> GiveUpInGame (get_names_from current_team)
        | Some get_card_from_discard_pile -> (
            let name_current_player =
              get_current_team_from board
              |> get_current_player_from |> get_name_from
            in

            if get_card_from_discard_pile then
              Format.printf
                "The player %s of the team %d took the card from the discard \
                 pile@ "
                name_current_player board.current_team_index
            else
              Format.printf
                "The player %s of team %d took the card from the draw pile@ "
                name_current_player board.current_team_index;
            let new_board_with_card_peeked =
              draw_card board current_team get_card_from_discard_pile
            in
            match play_move_player 3 new_board_with_card_peeked with
            | None -> GiveUpInGame (get_names_from current_team)
            | Some new_board ->
                let new_current_team = get_current_team_from new_board in
                if has_win new_current_team then (
                  Format.printf "End of the game, all teams :@ %a@ "
                    (fun fmt team_list ->
                      List.iter (fun t -> pp_team true true fmt t) team_list)
                    new_board.teams;
                  Win (get_names_from current_team))
                else round new_board)
      in
      try round board with
      | Invalid_play ->
          Error "The rules of the game were not followed 3 times in a row."
      | No_more_card -> Equality
      | _ -> Error "The game is broken.")
