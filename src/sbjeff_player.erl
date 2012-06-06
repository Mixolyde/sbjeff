%%%-------------------------------------------------------------------
%%% File    : sbjeff_player.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Utilities for handling the player and his hand
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_player).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

new_player() ->
  % creates a player with a random number in their name, (e.g. - "Player1234")
  new_player(random:uniform(9999)).
new_player(Int) when is_integer(Int) ->
  new_player("Player" ++ integer_to_list(Int));
new_player(String) when is_list(String), length(String) > 0 ->
  shuffle_up(#player{pname = String, cash = 50}).

shuffle_up(Player) ->
  {Hand, Deck} = lists:split(?HANDSIZE, sbjeff_cards:shuffled_deck()),
  Player#player{hand = Hand, deck = Deck}.

is_player(P) when is_record(P, player) ->
    true;
is_player(_P) ->
    false.

select_card(Player = #player{hand = Hand, deck = [FirstDeck | RestDeck]}, Card) ->
  % remove card from hand
  ReducedHand = lists:delete(Card, Hand),
  4 = length(ReducedHand),
  % replace card in hand from deck
  NewHand = [FirstDeck | ReducedHand],

  % return updated player
  Player#player{hand = NewHand, deck = RestDeck}.