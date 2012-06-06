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
card_record(rec) -> #card{cname=rec, display="Recreation", rank = 0, cost = -1};
card_record(doc) -> #card{cname=doc, display="Docking Bay", rank = 1, cost = -1};
card_record(com) -> #card{cname=com, display="Communication", rank = 2, cost = -1};
card_record(lab) -> #card{cname=lab, display="Laboratory", rank = 3, cost = 1};
card_record(fac) -> #card{cname=fac, display="Factory", rank = 4, cost = 1};
card_record(hab) -> #card{cname=hab, display="Habitat", rank = 5, cost = 2};
card_record(pow) -> #card{cname=pow, display="Power Station", rank = 6, cost = 3};
card_record(sab) -> #card{cname=sab, display="Sabotage", rank = 7, cost = 1};
card_record(Else) ->
  error({badarg, Else}).

played_card(_Pos, sab, _Pname, _Orient) ->
  error({badarg, "Cannot play a Sabotage card"});
played_card({X, Y}, Cname, Pname, Orient) ->
  case lists:member(Cname, ?CARDNAMES) of
    true ->
      PCard = #played_card{pos = {X, Y}, cname = Cname, pname = Pname, orientation = Orient},
      PCard;
    false ->
      error({badarg, "Bad cardname to play"})
  end.

%% return a list of valid exit directions for a card, given its orientation
% assume a cap's default exit orientation is the direction it's facing
card_exits(Card, 1) when Card == rec; Card == doc; Card == com -> [east];
card_exits(Card, 2) when Card == rec; Card == doc; Card == com -> [south];
card_exits(Card, 3) when Card == rec; Card == doc; Card == com -> [west];
card_exits(Card, 4) when Card == rec; Card == doc; Card == com -> [north];
card_exits(pow, _Any) -> [north, south, east, west];
card_exits(sab, _Any) -> error({badarg, sab}).

is_card(C) when is_record(C, card) ->
    true;
is_card(_C) ->
    false.