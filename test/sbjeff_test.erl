%%%-------------------------------------------------------------------
%%% File    : sbjeff_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Calls all unit test modules
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

%% asserts each unit test module returns ok
%% returns ok or {error, Message} if crashes
unit_test() ->
  ok =  sbjeff_cards_test:unit_test(),
  ok = sbjeff_player_test:unit_test(),
  ok =   sbjeff_game_test:unit_test(),
  ok.