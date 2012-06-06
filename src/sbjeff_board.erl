%%%-------------------------------------------------------------------
%%% File    : sbjeff_board.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Utilities for handling the board data
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_board).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

-define(TABLE, ?MODULE).

%%% Each process must hold onto a handle to its Table
new_board() ->
  try ets:delete(?TABLE) of
    true -> ok
  catch
    _:_ -> ok
  end,
  ets:new(?TABLE, [set, protected, {keypos, #played_card.pos}]).

delete_board(Table) -> ets:delete(Table).

reset_board(Table) -> ets:delete_all_objects(Table).

insert_card(Table, Card) when is_record(Card, played_card) ->
  case valid_move(Table, Card) of
    true ->
      true = ets:insert(Table, Card);
    false ->
      error({badarg, "Invalid card move"})
  end.

valid_move(_Table, Card) when is_record(Card, played_card) ->
  true.

valid_sab(_Table, {X, Y}) ->
  io:format("Checking board for valid sabotage placement at {~b, ~b}~n", [X, Y]),
  true.

card_count(Table) ->
  Count = ets:select_count(Table, [ {#played_card{_ = '_'}, [], [true]} ]),
  % io:format("Select returned count: ~b~n", [Count]),
  Count.

is_capped(_Table) ->
  % use dijkstra to follow all paths
  false.

