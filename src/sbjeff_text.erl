%%%-------------------------------------------------------------------
%%% File    : sbjeff_text.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Text-based game interface, mostly for testing
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_text).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

-define(IODevice, standard_io).

start() ->
  % get humans
  {HumanCount, CompCount} = get_player_counts(),

  % get human names
  PlayerNames = get_player_names(HumanCount, []),
  CompNames = get_comp_names(CompCount, PlayerNames, []),
  true = (length(PlayerNames) + length(CompNames)) >= 2,
  true = (length(PlayerNames) + length(CompNames)) =< 4,
  % TODO: start game with computer players
  start(PlayerNames).


start(PlayerNames) ->
  PlayerRecords = [sbjeff_player:new_player(PlayerName) || PlayerName <- PlayerNames],
  io:format("Created Records: ~p~n", [PlayerRecords]),
  % start session
  Session = sbjeff_round:new_session(PlayerRecords),
  % start rounds
  ok = round_loop(Session),
  io:format("Thanks for playing!~n"),
  print_session_stats(Session),
  ok.


get_player_counts() ->
  HumanCount = num_prompt("Number of human players [1-4]? ", 1, 4),
  MinComp = max(0, 2 - HumanCount),
  MaxComp = (4 - HumanCount),
  if
    MinComp /= MaxComp ->       % true block
      Prompt = lists:flatten(io_lib:format("Number of computer players [~b-~b]? ", [MinComp, MaxComp])),
      CompCount = num_prompt(Prompt, MinComp, MaxComp);
    true -> CompCount = MinComp % else block
  end,
  {HumanCount, CompCount}.

num_prompt(TextPrompt, Min, Max) ->
  % prompt for int
  Data = get_user_input(TextPrompt),
  try list_to_integer(Data) of
    Integer when Integer >= Min, Integer =< Max -> Integer;
    Integer ->
      io:format("~b not within bounds.~n", [Integer]),
      num_prompt(TextPrompt, Min, Max)
  catch
    _:_ ->
      io:format("Not a valid number, try again.~n"),
      num_prompt(TextPrompt, Min, Max)
  end.

name_prompt(NamePrompt, Names) ->
  Name = get_user_input(NamePrompt),
  case lists:member(Name, Names) of
    true ->
      io:format("~s already taken, try again.~n", [Name]),
      name_prompt(NamePrompt, Names);
    false -> Name
  end.

yorn_prompt() ->
  Result = get_user_input("Play another game (Y/N)? "),
  case lists:member(Result, ["y", "n", "Y", "N", "yes", "no", "YES", "NO"]) of
    true ->
      case lists:member(Result, ["y", "Y", "yes", "YES"]) of
        true -> yes;
        false -> no
      end;
    false ->
      io:format("Invalid reponse.~n"),
      yorn_prompt()
  end.

get_user_input(Prompt) ->
  string:strip(   % remove spaces from front and back
    string:strip( % remove line-feed from the end
      io:get_line(?IODevice, Prompt), right, $\n)).

get_player_names(0, Accum) -> lists:reverse(Accum);
get_player_names(Num, Accum) ->
  Prompt = lists:flatten(io_lib:format("Enter name for player ~b: ", [length(Accum) + 1])),
  Name = name_prompt(Prompt, Accum),
  get_player_names(Num - 1, [Name | Accum]).

get_comp_names(0, _PlayerNames, Accum) -> Accum;
get_comp_names(Num, PlayerNames, Accum) ->
  CompName = "Computer" ++ integer_to_list(random:uniform(9999)),
  case lists:member(CompName, PlayerNames) of
    true -> get_comp_names(Num, PlayerNames, Accum);
    false -> get_comp_names(Num - 1, [CompName | PlayerNames], [CompName | Accum])
  end.

round_loop(Session) when is_record(Session, session) ->
  % play a game
  UpdatedSession = start_round(Session),
  % print end of game stats
  print_round_stats(Session),
  % prompt for another game
  case yorn_prompt() of
    yes -> round_loop(UpdatedSession);
    no -> ok
  end.

start_round(Session) when is_record(Session, session) ->
  % start a new game
  Round = sbjeff_round:new_round(),
  % give players new decks/hands
  ShuffledPlayers = [sbjeff_player:shuffle_up(Player) || Player <- Session#session.players],
  NewSession = Session#session{players = ShuffledPlayers, round = Round},
  SessionAfterRound = turn_loop(NewSession),
  % clean up
  sbjeff_board:delete_board((SessionAfterRound#session.round)#round.boardtable),
  SessionAfterRound.

turn_loop(Session) ->
  % turn loop
  SessionAfterTurn = single_turn(Session),
  % if game over, end game
  SessionAfterTurn.
  % else, go to next turn
  %turn_loop(Session, GameAfterTurn).

single_turn(Session) ->
  % get card selections
  PlayerSelections = get_selections(Session),       % selecting a card will update the hand/deck of the Player
  Players = [ Player || {Player, _Selection} <- PlayerSelections ],

  % determine order and deferred cards
  % for each player, play chosen and/or deferred cards
  % check for close of station in each turn
  % if closed end the game
  % else, next player
  % after all players, update session and return
  Session#session{players = Players}.

get_selections(#session{players = Players,
  round = Round}) ->
  % get selection for each player in the list
  get_selections(Players, Round, []).

get_selections([], _Round, Selections) -> Selections;
get_selections([FirstPlayer | RestPlayers], Round, Selections) ->
  % show prompt
  % update player hand
  % add selection to list and recurse
  get_selections(RestPlayers, Round, [{FirstPlayer, sab} | Selections]).

print_board(_Table) ->
  io:format("Printing board state~n"),
  ok.


print_round_stats(Session) ->
  print_stats(Session, "Round Over. Current Scores:").

print_session_stats(Session) ->
  print_stats(Session, "Session Over. Final Scores:").

print_stats(Session, Message) ->
  io:format("~s~n", [Message]),
  lists:foreach(fun(PlayerRecord) ->
    io:format("~s: ~b~n", [PlayerRecord#player.pname, PlayerRecord#player.cash]) end,
    Session#session.players),
  ok.

%print_hand()
%   rec  lab  pow  sab
%         |    |   \|/
%    O-   +-  -+-  -*-
%              |   /|\
% R:  1    3    6    7
% C: -1    1    3    1
print_hand(PlayerName, Hand) ->
  % format the info into lines
  Records = [sbjeff_cards:card_record(Card) || Card <- Hand],
  % print them
  Titles = "  " ++ string:join([io_lib:format("~5s", [Card]) || Card <- Hand], ""),
  Priorities = "R:" ++ string:join([io_lib:format("~5b", [Record#card.rank]) || Record <- Records], ""),
  Costs = "C:" ++ string:join([io_lib:format("~5b", [Record#card.cost]) || Record <- Records], ""),
  Row1 = "  " ++ string:join([io_lib:format("~5s", [card_display(Card, 1, 1)]) || Card <- Hand], ""),
  Row2 = "  " ++ string:join([io_lib:format("~5s", [card_display(Card, 2, 1)]) || Card <- Hand], ""),
  Row3 = "  " ++ string:join([io_lib:format("~5s", [card_display(Card, 3, 1)]) || Card <- Hand], ""),
  io:format("~s's Hand:~n", [PlayerName]),
  io:format("~s~n", [Titles]),
  io:format("~s~n", [Row1]),
  io:format("~s~n", [Row2]),
  io:format("~s~n", [Row3]),
  io:format("~s~n", [Priorities]),
  io:format("~s~n", [Costs]),
  ok.

% cardname, row to print, orientation 1-4
card_display(Card, 1, 1) when Card == rec; Card == doc; Card == com -> "   ";
card_display(Card, 1, 2) when Card == rec; Card == doc; Card == com -> "   ";
card_display(Card, 1, 3) when Card == rec; Card == doc; Card == com -> "   ";
card_display(Card, 1, 4) when Card == rec; Card == doc; Card == com -> " | ";
card_display(Card, 2, 1) when Card == rec; Card == doc; Card == com -> " O-";
card_display(Card, 2, 2) when Card == rec; Card == doc; Card == com -> " O ";
card_display(Card, 2, 3) when Card == rec; Card == doc; Card == com -> "-O ";
card_display(Card, 2, 4) when Card == rec; Card == doc; Card == com -> " O ";
card_display(Card, 3, 1) when Card == rec; Card == doc; Card == com -> "   ";
card_display(Card, 3, 2) when Card == rec; Card == doc; Card == com -> " | ";
card_display(Card, 3, 3) when Card == rec; Card == doc; Card == com -> "   ";
card_display(Card, 3, 4) when Card == rec; Card == doc; Card == com -> "   ";

card_display(lab, 1, 1) -> " | ";
card_display(lab, 2, 1) -> " +-";
card_display(lab, 3, 1) -> "   ";
card_display(lab, 1, 2) -> "   ";
card_display(lab, 2, 2) -> " +-";
card_display(lab, 3, 2) -> " | ";
card_display(lab, 1, 3) -> "   ";
card_display(lab, 2, 3) -> "-+ ";
card_display(lab, 3, 3) -> " | ";
card_display(lab, 1, 4) -> " | ";
card_display(lab, 2, 4) -> "-+ ";
card_display(lab, 3, 4) -> "   ";

card_display(fac, 1, Orient) when Orient == 1; Orient == 3 -> " | ";
card_display(fac, 2, Orient) when Orient == 1; Orient == 3 -> " + ";
card_display(fac, 3, Orient) when Orient == 1; Orient == 3 -> " | ";
card_display(fac, 1, Orient) when Orient == 2; Orient == 4 -> "   ";
card_display(fac, 2, Orient) when Orient == 2; Orient == 4 -> "-+-";
card_display(fac, 3, Orient) when Orient == 2; Orient == 4 -> "   ";

card_display(hab, 1, 1) -> " | ";
card_display(hab, 2, 1) -> " +-";
card_display(hab, 3, 1) -> " | ";
card_display(hab, 1, 2) -> "   ";
card_display(hab, 2, 2) -> "-+-";
card_display(hab, 3, 2) -> " | ";
card_display(hab, 1, 3) -> " | ";
card_display(hab, 2, 3) -> "-+ ";
card_display(hab, 3, 3) -> " | ";
card_display(hab, 1, 4) -> " | ";
card_display(hab, 2, 4) -> "-+-";
card_display(hab, 3, 4) -> "   ";

card_display(pow, 1, _Orient) -> " | ";
card_display(pow, 2, _Orient) -> "-+-";
card_display(pow, 3, _Orient) -> " | ";
card_display(sab, 1, _Orient) -> "\\|/";
card_display(sab, 2, _Orient) -> "-*-";
card_display(sab, 3, _Orient) -> "/|\\".



%   |  |
%   +--+-
%  1| 2|
%   |
%   o