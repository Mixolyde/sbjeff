%%%-------------------------------------------------------------------
%%% File    : sbjeff.app
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Starbase Jeff app resource file
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------
%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sbjeff,
  [{description,  "An implementation of starbase jeff written in Erlang" },
   {vsn,           "1.0" },
   {modules,       [sbjeff_cards, sbjeff_player, sbjeff_game]},
   {registered,    []},
   {applications,  [kernel,stdlib]},
   {env, []}
  ]
}.
