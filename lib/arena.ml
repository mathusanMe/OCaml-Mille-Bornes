open Teams_engine
open Board_engine
open Cards_engine

type endplay = Win of team | GiveUpInGame of team | GiveUpInit

let pp_endplay _ _ = (* TODO *) ()
let initial_bot_choose_card_to_play _ _ _ = (* TODO *) (0, None)
let initial_bot_want_to_peek_discard_pile _ _ _ _ = (* TODO *) true
let initial_bot_want_to_play_coup_fourre _ _ _ _ = (* TODO *) true

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

let play_move_player b = (* TODO *) Some b
let arena () = (* TODO *) ()
