open Cards_engine

type player_struct = { name : string; hand : deck_of_card }
type player = Computer of player_struct | Human of player_struct

type driving_zone = {
  speed_limit_pile : pile_of_card;
  drive_pile : pile_of_card;
  can_drive : bool;
  distance_cards : deck_of_card;
  score : int;
  safety_area : deck_of_card;
  coup_fouree_cards : deck_of_card;
}

type team = {
  players : player list;
  board : driving_zone;
  current_player_index : int;
}

let get_current_player_from (t : team) =
  List.nth t.players t.current_player_index

let set_next_player_from (t : team) =
  if List.length t.players = 2 then
    {
      players = t.players;
      board = t.board;
      current_player_index = 1 - t.current_player_index;
    }
  else t
