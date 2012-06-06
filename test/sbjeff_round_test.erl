%%%-------------------------------------------------------------------
%%% File    : sbjeff_round_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Tests the game utilities
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_round_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

unit_test() ->
  Game1 = sbjeff_round:new_round(),
  _BoardTable = Game1#round.boardtable,
  1 = Game1#round.turn_count,
  [] = Game1#round.deferred,
  0 = Game1#round.pot,
  FourPlayers = [sbjeff_player:new_player("Name" ++ integer_to_list(Int)) || Int <- lists:seq(1, 4)],
  4 = length(FourPlayers),
  % test bad arguments to new session
  {'EXIT',{{badarg, []}, _Stack}} = (catch sbjeff_round:new_session([])),
  {OnePlayer, ThreePlayers} = lists:split(1, FourPlayers),
  {'EXIT',{{badarg, OnePlayer}, _Stack2}} = (catch sbjeff_round:new_session(OnePlayer)),
  {'EXIT',{{badarg, thing}, _Stack3}} = (catch sbjeff_round:new_session(thing)),
  FivePlayers = FourPlayers ++ OnePlayer,
  {'EXIT',{{badarg, FivePlayers}, _Stack4}} = (catch sbjeff_round:new_session(FivePlayers)),
  % test session creation with 4 players
  Session = sbjeff_round:new_session(FourPlayers),
  4 = length(Session#session.players),
  0 = (Session#session.round)#round.pot,
  Session3 = sbjeff_round:new_session(ThreePlayers),
  3 = length(Session3#session.players),
  0 = (Session3#session.round)#round.pot,
  ok.