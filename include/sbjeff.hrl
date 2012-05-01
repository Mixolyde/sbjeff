%%%-------------------------------------------------------------------
%%% File    : sbjeff.hrl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Record and macro definitions common to sbjeff
%%%   modules
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Data Type: player and their state in a game
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

%%--------------------------------------------------------------------
%% Data Type: played_card represents a card played onto the game_board
%%   cname: short card name, an atom from ?CARDNAMES
%%   pname: playername that played the card
%%   x: horizontal grid location, a signed int
%%   y: vertical grid location, a signed int
%%   orientation: [1-4] indicating the orientation of the piece, 1 is north, 2 is east, etc
%%--------------------------------------------------------------------
-record(played_card, {cname, pname, x, y, orientation}).

% no idea what to use as a data structure to represent the grid-like
%   game board, yet.
-record(board, {}).

%%--------------------------------------------------------------------
%% Data Type: game, the current game or hand's state
%%   board: the board data structure represents the played cards
%%   deferred: the lists of player deferred cards {pname, [cards]}
%%   pot: current cash in the pot, an int
%%--------------------------------------------------------------------
-record(game, {board = #board{}, turn_count = 1, deferred = [], pot = 0}).

%%--------------------------------------------------------------------
%% Data Type: session, tracks the overall session of multiple games
%%   players: list of #players
%%   game: current #game state
%%--------------------------------------------------------------------
-record(session, {players = [], game = #game{}}).

%% list macro for iterating the list of card names
-define(CARDNAMES, [rec, doc, com, lab, fac, hab, pow, sab]).

%% size of a full deck
-define(DECKSIZE, 20).

%% size of a player's hand of cards
-define(HANDSIZE, 5).
