%%%-------------------------------------------------------------------
%%% File    : sbjeff_game_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Tests the game utilities
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_game_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

unit_test() ->
  Game1 = sbjeff_game:new_game(),
  true = is_record(Game1#game.board, board),
  1 = Game1#game.turn_count,
  [] = Game1#game.deferred,
  0 = Game1#game.pot,
  Players = [sbjeff_player:new_player("Name" ++ integer_to_list(Int)) || Int <- lists:seq(1, 4)],
  4 = length(Players),
  {error, illegal_arg, []} = sbjeff_game:new_session([]),
  {OnePlayer, ThreePlayers} = lists:split(1, Players),
  {error, illegal_arg, OnePlayer} = sbjeff_game:new_session(OnePlayer),
  {error, illegal_arg, thing} = sbjeff_game:new_session(thing),
  FivePlayers = Players ++ OnePlayer,
  {error, illegal_arg, FivePlayers} = sbjeff_game:new_session(FivePlayers),
  Session = sbjeff_game:new_session(Players),
  4 = length(Session#session.players),
  0 = (Session#session.game)#game.pot,
  Session3 = sbjeff_game:new_session(ThreePlayers),
  3 = length(Session3#session.players),
  0 = (Session3#session.game)#game.pot,
  ok.