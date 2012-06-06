%%%-------------------------------------------------------------------
%%% File    : sbjeff_board_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Test board state utils
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_board_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

unit_test() ->
  BoardTable = sbjeff_board:new_board(),
  0 = sbjeff_board:card_count(BoardTable),
  sbjeff_board:reset_board(BoardTable),
  0 = sbjeff_board:card_count(BoardTable),
  Card1 = sbjeff_cards:played_card({0, 0}, rec, "Brian", 1),
  Card2 = sbjeff_cards:played_card({0, 1}, rec, "Brian", 1),
  sbjeff_board:insert_card(BoardTable, Card1),
  1 = sbjeff_board:card_count(BoardTable),
  sbjeff_board:insert_card(BoardTable, Card2),
  2 = sbjeff_board:card_count(BoardTable),
  sbjeff_board:reset_board(BoardTable),
  0 = sbjeff_board:card_count(BoardTable),
  ok.