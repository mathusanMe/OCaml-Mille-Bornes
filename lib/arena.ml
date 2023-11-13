open Teams_engine
open Board_engine
open Cards_engine

type endplay = Win of team | GiveUpInGame of team | GiveUpInit

let pp_endplay _ _ = (* TODO *) ()
let initial_bot_choose_card_to_play _ _ _ = (* TODO *) Some (0, None)
let initial_bot_want_to_peek_discard_pile _ _ _ _ = (* TODO *) Some true
let initial_bot_want_to_play_coup_fourre _ _ _ _ = (* TODO *) Some true

let initial_strategy =
  {
    name = "Initial strategy";
    choose_card_to_play = initial_bot_choose_card_to_play;
    want_to_peek_discard_pile = initial_bot_want_to_peek_discard_pile;
    want_to_play_coup_fourre = initial_bot_want_to_play_coup_fourre;
  }

let strategy_list = [ initial_strategy ]
let pp_strategy_list _ _ = ()

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
  let strategy_list_length = List.length strategy_list in
  if strategy_list_length = 0 then None
  else if strategy_list_length = 1 then (
    let strategy = List.hd strategy_list in
    Format.printf
      "As it is alone, the strategy chosen for the bot %d of the team %d is %s@;"
      num id strategy.name;
    Some strategy)
  else
    match
      request_id
        (Format.asprintf
           "With which strategy the bot %d of team %d will play considering \
            the list above ?"
           num id)
        (List.length strategy_list)
    with
    | None -> None
    | Some i -> Some (List.nth strategy_list i)

let rec ask_player_info id num already_created_team =
  let max_name_length = 15 in
  match
    request_answer_with_length
      (Format.asprintf "Give the name of the player %d of the team %d" num id)
      max_name_length
  with
  | None -> None
  | Some name -> (
      if does_player_have_this_name_in_team_list name already_created_team then (
        Format.printf
          "This name is already taken by someone else, take another.@;";
        ask_player_info id num already_created_team)
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

let init_team id is_team_of_two already_created_team =
  match ask_player_info id 1 already_created_team with
  | None -> None
  | Some (name1, None) -> (
      if not is_team_of_two then Some (init_team_with_one_human name1 id)
      else
        match ask_player_info id 2 already_created_team with
        | None -> None
        | Some (name2, None) -> Some (init_team_with_two_human name1 name2 id)
        | Some (name2, Some strategy) ->
            Some
              (init_team_with_one_human_and_one_computer name1 false name2 true
                 strategy id))
  | Some (name1, Some strategy1) -> (
      if not is_team_of_two then
        Some (init_team_with_one_computer name1 strategy1 id)
      else
        match ask_player_info id 2 already_created_team with
        | None -> None
        | Some (name2, None) ->
            Some
              (init_team_with_one_human_and_one_computer name1 true name2 false
                 strategy1 id)
        | Some (name2, Some strategy2) ->
            Some
              (init_team_with_two_computer name1 strategy1 name2 strategy2 id))

let init_teams () =
  match request_number_of_player () with
  | None -> None
  | Some (nb_player, is_team_of_two) -> (
      let rec aux_init_teams acc_team acc_id =
        if acc_id >= nb_player then Some (List.rev acc_team)
        else
          let new_team = init_team acc_id is_team_of_two acc_team in
          match new_team with
          | None -> None
          | Some t ->
              if is_team_of_two then aux_init_teams (t :: acc_team) (acc_id + 2)
              else aux_init_teams (t :: acc_team) (acc_id + 1)
      in
      match aux_init_teams [] 0 with
      | None -> None
      | Some team_list ->
          Format.printf "Team created : %a@;" pp_names_of_team_list team_list;
          Some team_list)

let init_board () =
  match init_teams () with
  | None -> None
  | Some teams_of_board -> (
      match
        request_id
          "Which of the id teams has the youngest player (or which team should \
           start)?"
          (List.length teams_of_board - 1)
      with
      | None -> None
      | Some id ->
          Some
            {
              draw_pile = generate_initial_pile () |> shuffle_pile;
              discard_pile = [];
              teams = teams_of_board;
              current_team_index = id;
            })

let player_teletype_choose_card_to_play p pi pi_list =
  let p_struct = get_player_struct_from p in
  Format.printf
    "%s of team %d, your score is %d, your hand and your driving zone are :@;\
     %a@ @[<v>%a@]@;"
    p_struct.name pi.id pi.score (pp_deck_of_card "Hand") p_struct.hand
    pp_public_informations pi;
  match
    request_id
      "Choose the id of the card you want to play, or discard, in your hand \
       (the number in front of it)"
      (List.length p_struct.hand - 1)
  with
  | None -> None
  | Some id_card -> (
      match request_yes_or_no "Do you want to discard it ?" with
      | None -> None
      | Some true -> Some (id_card, None)
      | Some false -> (
          match List.nth p_struct.hand id_card with
          | Hazard _ -> (
              Format.printf "Here are all the existing zones : @ %a"
                pp_public_informations_list pi_list;
              match
                request_id "Choose the id of the one you want to attack."
                  (List.length pi_list - 1)
              with
              | None -> None
              | Some id_pi -> Some (id_card, Some (List.nth pi_list id_pi).id))
          | _ -> Some (id_card, Some pi.id)))

let player_teletype_want_to_peek_discard_pile p c pi _ =
  let p_struct = get_player_struct_from p in
  Format.printf
    "%s of team %d, your score is %d, your hand and your driving zone are :@;\
     %a@ @[<v>%a@]@;"
    p_struct.name pi.id pi.score (pp_deck_of_card "Hand") p_struct.hand
    pp_public_informations pi;
  request_yes_or_no
    (Format.asprintf
       "Do you want to pick the card %a of the discard pile ? If not you'll \
        take a random card in draw pile "
       pp_card c)

let player_teletype_want_to_play_coup_fourre p h pi _ =
  let p_struct = get_player_struct_from p in
  Format.printf
    "%s of team %d, your team is attacked by the card %a, your score is %d, \
     your hand and your driving zone are :@;\
     %a\n\
    \      @ @[<v>%a@]@;"
    p_struct.name pi.id pp_card (Hazard h) pi.score (pp_deck_of_card "Hand")
    p_struct.hand pp_public_informations pi;
  request_yes_or_no
    "Do you want to play your safety and do a coup fourre to earn 200 points ?"

let get_list_of_other_public_information_than (p_info : public_informations)
    (b : board) =
  List.rev
    (List.fold_left
       (fun acc t ->
         let t_p_info = t.shared_public_informations in
         if p_info = t_p_info then acc else t_p_info :: acc)
       [] b.teams)

let play_move (current_player : player) (current_team : team) (b : board) =
  match current_player with
  | Computer (_, p_strat) ->
      p_strat.choose_card_to_play current_player
        current_team.shared_public_informations
        (get_list_of_other_public_information_than
           current_team.shared_public_informations b)
  | Human _ ->
      player_teletype_choose_card_to_play current_player
        current_team.shared_public_informations
        (get_list_of_other_public_information_than
           current_team.shared_public_informations b)

exception Invalid_id_public_information
exception Computer_invalid_move
(*the player played a prohibited move*)

exception Discard_card_error
exception Place_card_error
(*The game is broken*)

let try_get_team_corresponding_public_information (id : int) (l : team list) =
  try List.find (fun t -> t.shared_public_informations.id = id) l
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
           target_team.players)
    with Not_found -> None
  in
  match player_have_counter with
  | None -> Some new_board_after_place_hazard_card
  | Some player_have_counter -> (
      let necessary_safety =
        match hazard with
        | Stop | SpeedLimit -> EmergencyVehicle
        | OutOfGas -> FuelTruck
        | FlatTire -> PunctureProof
        | Accident -> DrivingAce
      in
      match
        match player_have_counter with
        | Computer (_, p_strat) ->
            p_strat.want_to_play_coup_fourre player_have_counter hazard
              target_team.shared_public_informations
              (get_list_of_other_public_information_than
                 target_team.shared_public_informations
                 previous_board_befor_place_hazard_card)
        | Human _ ->
            player_teletype_want_to_play_coup_fourre player_have_counter hazard
              target_team.shared_public_informations
              (get_list_of_other_public_information_than
                 target_team.shared_public_informations
                 previous_board_befor_place_hazard_card)
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
              raise Place_card_error
          in
          let new_board =
            try
              discard_card_from_player new_board current_team
                (get_current_player_from current_team)
                card_used
            with
            | Team_not_found | Player_not_found | Empty_deck | Card_not_found ->
              raise Place_card_error
          in
          let () =
            Format.printf
              "Player %s on team %d has used %a on coup fouree. The card %a go \
               on discard pile.@ "
              (get_player_struct_from player_have_counter).name
              id_of_target_public_informations pp_card (Safety necessary_safety)
              pp_card card_used
          in
          Some new_board
      | Some false ->
          let () =
            Format.printf
              "Player %s on team %d has decide to not use %a on coup fouree.@ "
              (get_player_struct_from player_have_counter).name
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
    Format.printf "Player %s has use %a on team %d@ "
      (get_player_struct_from current_player).name pp_card card_used
      id_of_target_public_informations
  in
  match card_used with
  | Hazard hazard ->
      try_use_coup_fouree b new_board current_team card_used hazard target_team
        id_of_target_public_informations
  | _ -> Some new_board

let rec play_move_player b =
  let retry_play_move_player (b : board) (current_player : player) =
    match current_player with
    | Computer (p_struct, _) ->
        let () =
          Format.printf "Computer %s has try an invalid move@ " p_struct.name
        in
        if
          request_yes_or_no
            "The bot was unable to make a move due to an error on its part. \
             Should the game be stopped?"
          = Some true
        then play_move_player b
        else raise Computer_invalid_move
    | Human p_struct ->
        let () =
          Format.printf "Human %s have try an invalid move.@ " p_struct.name
        in
        play_move_player b
  in
  let current_team = get_current_team_from b in
  let current_player = get_current_player_from current_team in
  let move = play_move current_player current_team b in
  match move with
  | None -> None (*give up case*)
  | Some (id_card_on_deck_of_current_player, id_of_target_public_informations)
    -> (
      match id_card_on_deck_of_current_player with
      | id
        when 0 <= id
             && id < List.length (get_player_struct_from current_player).hand
        -> (
          let card_used =
            nth_hand_player current_player id_card_on_deck_of_current_player
          in
          match id_of_target_public_informations with
          | None ->
              (*disard card case*)
              try_discard_card b current_team card_used
          | Some id_of_target_public_informations -> (
              (*place card case*)
              try
                try_place_card b current_team current_player card_used
                  id_of_target_public_informations
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

let arena () = (* TODO *) ()
