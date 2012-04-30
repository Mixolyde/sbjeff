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
  lists:duplicate(3, rec) ++
  lists:duplicate(2, doc) ++
  lists:duplicate(3, com) ++
  lists:duplicate(4, lab) ++
  lists:duplicate(3, fac) ++
  lists:duplicate(2, hab) ++
  lists:duplicate(1, pow) ++
  lists:duplicate(2, sab).

% return a new, twice shuffled deck
shuffled_deck() ->
  % random:seed(now()),
  Deck = sorted_deck(),
  Shuffle1 = shuffle_deck(Deck),
  shuffle_deck(Shuffle1).

shuffle_deck(Deck) ->
  shuffle_deck(Deck, []).
shuffle_deck([], Accum) -> Accum;
shuffle_deck(Deck, Accum) ->
  %pull a random card out of the deck, add to accum and recurse
  Rand = random:uniform(length(Deck)),
  Card = lists:nth(Rand, Deck),
  NewDeck = lists:delete(Card, Deck),
  shuffle_deck(NewDeck, [Card | Accum]).

%% return the record details of a particular card
card_record(rec) -> #card{cname=rec, display="Recreation", priority = 0, cost = -1};
card_record(doc) -> #card{cname=doc, display="Docking Bay", priority = 1, cost = -1};
card_record(com) -> #card{cname=com, display="Communication", priority = 2, cost = -1};
card_record(lab) -> #card{cname=lab, display="Laboratory", priority = 3, cost = 1};
card_record(fac) -> #card{cname=fac, display="Factory", priority = 4, cost = 1};
card_record(hab) -> #card{cname=hab, display="Habitat", priority = 5, cost = 2};
card_record(pow) -> #card{cname=pow, display="Power Station", priority = 6, cost = 3};
card_record(sab) -> #card{cname=sab, display="Sabotage", priority = 7, cost = 1};
card_record(Else) ->
  error({badarg, Else}).

is_card(C) when is_record(C, card) ->
    true;
is_card(_C) ->
    false.