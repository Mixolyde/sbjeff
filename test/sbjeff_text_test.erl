%%%-------------------------------------------------------------------
%%% File    : sbjeff_text_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Run input through the text based methods
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_text_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

unit_test() ->
  CompNames = sbjeff_text:get_comp_names(4, [], []),
  4 = length(CompNames),
  io:format("CompNames: ~p~n", [CompNames]),
  Hand = [sab, rec, doc, com, lab, fac, hab, pow],
  ok = sbjeff_text:print_hand("Player1", Hand),
  PlayerNames = ["Brian", "Rob", "George", "Michael"],
  PlayerRecords = [sbjeff_player:new_player(PlayerName) || PlayerName <- PlayerNames],
  Session = sbjeff_game:new_session(PlayerRecords),
  ok = sbjeff_text:print_round_stats(Session),
  ok = sbjeff_text:print_session_stats(Session),


  ok.