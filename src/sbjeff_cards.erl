%%%-------------------------------------------------------------------
%%% File    : sbjeff_cards.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Utilities for handling the deck of cards in the
%%%   game
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_cards).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

%%--------------------------------------------------------------------
%% Function: sorted_deck
%% Purpose: Return a new Deck structure, unshuffled
%% Args: None
%% Returns: A list of #cards
%%--------------------------------------------------------------------
sorted_deck() ->
  lists:duplicate(3, #card{cname=rec, display="Recreation", priority = 0, cost = -1}) ++
  lists:duplicate(2, #card{cname=doc, display="Docking Bay", priority = 1, cost = -1}) ++
  lists:duplicate(3, #card{cname=com, display="Communication", priority = 2, cost = -1}) ++
  lists:duplicate(4, #card{cname=lab, display="Laboratory", priority = 3, cost = 1}) ++
  lists:duplicate(3, #card{cname=fac, display="Factory", priority = 4, cost = 1}) ++
  lists:duplicate(2, #card{cname=hab, display="Habitat", priority = 5, cost = 2}) ++
  lists:duplicate(1, #card{cname=pow, display="Power Station", priority = 6, cost = 3}) ++
  lists:duplicate(2, #card{cname=sab, display="Sabotage", priority = 7, cost = 1}).

% return a new, shuffled deck
shuffled_deck() ->
  Deck = sorted_deck(),
  Shuffle1 = shuffle_deck(Deck),
  shuffle_deck(Shuffle1).

shuffle_deck(Deck) ->
  shuffle_deck(Deck, []).
shuffle_deck([], Accum) -> Accum;
shuffle_deck(Deck, Accum) ->
  %pull a random card out of the deck, add to accum and recurse
  Rand = 1,
  Card = lists:nth(Rand, Deck),
  NewDeck = lists:delete(Card, Deck),
  shuffle_deck(NewDeck, [Card | Accum]).