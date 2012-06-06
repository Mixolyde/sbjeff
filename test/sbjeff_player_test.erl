%%%-------------------------------------------------------------------
%%% File    : sbjeff_player_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Tests the player utilities
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(sbjeff_player_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../include/sbjeff.hrl").

unit_test() ->
  Player1 = sbjeff_player:new_player(1),
  "Player1" = Player1#player.pname,
  50 = Player1#player.cash,
  #player{pname = "String", hand = Hand2, deck = Deck2, cash = 50} =
    sbjeff_player:new_player("String"),
  ?HANDSIZE = length(Player1#player.hand),
  ?DECKSIZE - ?HANDSIZE = length(Player1#player.deck),
  ?HANDSIZE = length(Hand2),
  ?DECKSIZE - ?HANDSIZE = length(Deck2),
  true = sbjeff_player:is_player(Player1),
  false = sbjeff_player:is_player(player),
  Player2 = sbjeff_player:shuffle_up(Player1),
  "Player1" = Player2#player.pname,
  50 = Player2#player.cash,
  % select the first card in the player's hand and test the update
  [FirstHand | RestHand] = Player2#player.hand,
  [FirstDeck | RestDeck] = Player2#player.deck,
  Player3 = sbjeff_player:select_card(Player2, FirstHand),
  [FirstDeck | RestHand] = Player3#player.hand,
  RestDeck = Player3#player.deck,
  ok.