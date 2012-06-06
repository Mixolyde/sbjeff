%%%-------------------------------------------------------------------
%%% File    : sbjeff_ai_random_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Tests the random ai
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_ai_random_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

unit_test() ->
  Player1 = sbjeff_player:new_player(1),
  rec = sbjeff_ai_random:get_selection(Player1#player{hand=[sab, sab, rec, doc, hab]}, #round{turn_count=1}),
  sab = sbjeff_ai_random:get_selection(Player1#player{hand=[sab, sab, rec, doc, hab]}, #round{turn_count=2}),
  ok.