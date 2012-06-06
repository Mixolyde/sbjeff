%%%-------------------------------------------------------------------
%%% File    : sbjeff_ai_random.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : A computer player that makes completely random choices,
%%%   mostly for testing purposes.
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_ai_random).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

get_selection(#player{hand = Hand},
  #round{turn_count = 1}) ->
  % don't play a sabotage on turn 1
  % take from the hand until it's not a sabotage, return that choice
  [FirstNonSab | _RestHand] = lists:dropwhile(fun(Card) -> Card == sab end, Hand),
  FirstNonSab;
get_selection(#player{hand = [FirstCard | _Rest]}, _Round) ->
  FirstCard.
