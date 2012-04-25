%%%-------------------------------------------------------------------
%%% File    : sbjeff.hrl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Record and macro definitions common to sbjeff
%%%   modules
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-record(player, {pname, hand = [], deck = [], cash = 0}).

%% Card object. cname is an atom for the card name, display is the display name,
%%   priority is the order number on the card,
%%   cost is cost to play (-1 for caps)
-record(card, {cname, display, priority, cost}).

-record(game, {board, deferred = [], pot = 0}).

-record(session, {players = [], game}).

-define(CARDNAMES, [rec, doc, com, lab, fac, hab, pow, sab]).
