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
  PlayerRecords = [sbjeff_player:new_player(PlayerName) || PlayerName <- PlayerNames],
  io:format("Created Records: ~p~n", [PlayerRecords]),
  % start session
  Session = sbjeff_game:new_session(PlayerRecords),
  % start games
  ok = game_loop(Session),
  io:format("Thanks for playing!~n"),
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

game_loop(Session) when is_record(Session, session) ->
  % play a game
  UpdatedSession = play_game(Session),
  % print end of game stats
  % prompt for another game
  case yorn_prompt() of
    yes -> game_loop(UpdatedSession);
    no -> ok
  end.

play_game(Session) when is_record(Session, session) ->
  % get card selections
  % determine order and deferred cards
  % for each player, play chosen and/or deferred cards
  % check for close of station in each turn
  % if closed end the game
  % else, next player
  % after all players, back to card selections
  ok.