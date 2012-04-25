%%%-------------------------------------------------------------------
%%% File    : sbjeff.hrl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Record and macro definitions common to sbjeff
%%%   modules
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Data Type: player
%%   pname: player's display name, a string
%%   hand: current hand, a list of #cards
%%   deck: player's unplayed deck, a list of #cards
%%   cash: player's current cash, an int
%%--------------------------------------------------------------------
-record(player, {pname, hand = [], deck = [], cash = 0}).

%%--------------------------------------------------------------------
%% Data Type: card
%%   cname: short card name, an atom from ?CARDNAMES
%%   display: display name, a string
%%   priority: the turn priority when played, an int
%%   cost: cost to play, -1 for caps, an int
%%--------------------------------------------------------------------
-record(card, {cname, display, priority, cost}).

-record(game, {board, deferred = [], pot = 0}).

-record(session, {players = [], game}).

-define(CARDNAMES, [rec, doc, com, lab, fac, hab, pow, sab]).
