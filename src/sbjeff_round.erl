%%%-------------------------------------------------------------------
%%% File    : sbjeff_round.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Utilities for handling the state of a single round
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_round).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

new_round() ->
  % creates a new game with empty structures
  #round{boardtable = sbjeff_board:new_board(), turn_count = 1, deferred = [], pot = 0}.

new_session(Players) when is_list(Players), length(Players) > 1, length(Players) < 5 ->
  true = lists:all(fun(Player) -> is_record(Player, player) end, Players),
  #session{players = Players};
new_session(Else) ->
  error({badarg, Else}).

apply_card(Player, Card, Round) when is_record(Round, round), is_record(Card, card), is_record(Player, player) ->
  % apply a card to the game state
  Round.
