open Mille_bornes.Teams_engine

let team_with_one_computer = init_team_with_one_player "Computer" true
let team_with_one_human = init_team_with_one_player "Thomas" false

let team_with_two_computers =
  init_team_with_two_players "Computer1" true "Computer2" true

let team_with_two_humans =
  init_team_with_two_players "Gabin" false "Thomas" false

let team_with_computer_human =
  init_team_with_two_players "Computer" true "Gabin" false

let team_with_human_computer =
  init_team_with_two_players "Gabin" false "Computer" true
