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
  ok.