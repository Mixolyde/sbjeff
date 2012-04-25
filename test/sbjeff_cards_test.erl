%%%-------------------------------------------------------------------
%%% File    : sbjeff_cards_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Tests the card utilities
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_cards_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

unit_test() ->
  20 = length(sbjeff_cards:sorted_deck()),
  ok.