%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_user_admin_odbc).
-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 %% Roster
	 add_jid_to_group/5,
	 delete_jid_from_group/5
	]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").

-include("logger.hrl").
-include("jlib.hrl").


%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).


%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).


%%%
%%% Register commands
%%%

commands() ->
    [
     #ejabberd_commands{name = add_jid_to_group, tags = [roster],
			desc = "Add jid to a group in a user's roster (supports ODBC)",
			module = ?MODULE, function = add_jid_to_group,
			args = [{localuser, binary}, {localserver, binary},
				{jid, binary}, {nick, binary},
				{group, binary}],
			result = {res, rescode}},
     #ejabberd_commands{name = delete_jid_from_group, tags = [roster],
			desc = "Delete a jid from a user's roster group (supports ODBC)",
			module = ?MODULE, function = delete_jid_from_group,
			args = [{localuser, binary}, {localserver, binary},
				{jid, binary},{nick,binary},
        {group, binary}],
			result = {res, rescode}}
    ].


%%%
%%% Roster
%%%

add_jid_to_group(LocalUser, LocalServer, JID, Nick, Group) ->
  %%?DEBUG("add_jid_to_group: SavedGroups ~p JIDGroups ~p JID ~p~n",[SavedGroups,JIDGroups, JID]),
  Res = ejabberd_odbc:sql_query(LocalServer,[<<"insert into rostergroups(username, jid, grp)  values ('">>,
      ejabberd_odbc:escape(LocalUser), <<"','">>,
      ejabberd_odbc:escape(JID), <<"','">>,
      ejabberd_odbc:escape(Group), <<"');">>]),
  case Res of
    {updated, _} ->
      SavedGroups = odbc_queries:get_rostergroup_by_jid(LocalServer, LocalUser, JID),
      JIDGroups = case SavedGroups of
                    {selected,[<<"grp">>],Groups} -> [Group | [ Grp || [Grp] <- Groups]];
                    _-> []
                  end,
      JID1 = jlib:string_to_jid(JID),
      %%?DEBUG("add_jid_to_group: SavedGroups ~p JIDGroups ~p JID ~p ODBC ~p~n",[SavedGroups,JIDGroups, JID, Res]),
      push_roster_item(LocalUser, LocalServer, JID1#jid.luser, JID1#jid.lserver, {add, Nick, <<"both">>, JIDGroups}),
      ok;
    _-> error
  end.

delete_jid_from_group(LocalUser, LocalServer, JID, Nick, Group) ->
%%  ?DEBUG("add_jid_to_group: Groups - ~p~n",[Groups]),
  Res = ejabberd_odbc:sql_query(LocalServer,[<<"delete from rostergroups where username = '">>,
    ejabberd_odbc:escape(LocalUser), <<"' and jid = '">>,
    ejabberd_odbc:escape(JID), <<"' and grp = '">>,
    ejabberd_odbc:escape(Group), <<"';">>]),
  %%?DEBUG("delete_jid_from_group: ODBC res ~p~n",[Res]),
  case Res of
    {updated,_} ->
      SavedGroups = odbc_queries:get_rostergroup_by_jid(LocalServer, LocalUser, JID),
      JIDGroups = case SavedGroups of
                    {selected,[<<"grp">>],Groups} -> [ Grp || [Grp] <- Groups];
                    _-> []
                  end,
      JID1 = jlib:string_to_jid(JID),
      push_roster_item(LocalUser, LocalServer, JID1#jid.luser, JID1#jid.lserver, {add, Nick, <<"both">>, JIDGroups}),
      ok;
    _-> error
  end.

build_roster_item(U, S, {add, Nick, Subs, Groups}) ->
  {xmlel, <<"item">>,
    [{<<"jid">>, jlib:jid_to_string(jlib:make_jid(U, S, <<>>))},
      {<<"name">>, Nick},
      {<<"subscription">>, Subs}],
    %%[{xmlel, <<"group">>, [], [{xmlcdata, Group}]}]
     lists:map(fun(X) -> {xmlel,<<"group">>,[],[{xmlcdata,X}]} end, Groups)
    %% or [{xmlel,<<"group">>,[],[{xmlcdata,Group}]} || Group <- Groups]
  };
build_roster_item(U, S, remove) ->
  {xmlel, <<"item">>,
    [{<<"jid">>, jlib:jid_to_string(jlib:make_jid(U, S, <<>>))},
      {<<"subscription">>, <<"remove">>}],
    []
  }.
build_iq_roster_push(Item) ->
  {xmlel, <<"iq">>,
    [{<<"type">>, <<"set">>}, {<<"id">>, <<"push">>}],
    [{xmlel, <<"query">>,
      [{<<"xmlns">>, ?NS_ROSTER}],
      [Item]
    }
    ]
  }.

build_broadcast(U, S, {add, _Nick, Subs, _Group}) ->
  build_broadcast(U, S, list_to_atom(binary_to_list(Subs)));
build_broadcast(U, S, remove) ->
  build_broadcast(U, S, none);
%% @spec (U::binary(), S::binary(), Subs::atom()) -> any()
%% Subs = both | from | to | none
build_broadcast(U, S, SubsAtom) when is_atom(SubsAtom) ->
  {broadcast, {item, {U, S, <<>>}, SubsAtom}}.

push_roster_item(LU, LS, U, S, Action) ->
  lists:foreach(fun(R) ->
    push_roster_item(LU, LS, R, U, S, Action)
  end, ejabberd_sm:get_user_resources(LU, LS)).

push_roster_item(LU, LS, R, U, S, Action) ->
  LJID = jlib:make_jid(LU, LS, R),
  BroadcastEl = build_broadcast(U, S, Action),
  ejabberd_router:route(LJID, LJID, BroadcastEl),
  Item = build_roster_item(U, S, Action),
  ResIQ = build_iq_roster_push(Item),
  ejabberd_router:route(LJID, LJID, ResIQ).


%% %%
%% add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs) ->
%%     case add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs, []) of
%% 	{atomic, ok} ->
%% 	    push_roster_item(LocalUser, LocalServer, User, Server, {add, Nick, Subs, Group}),
%% 	    ok;
%% 	_ ->
%% 	    error
%%     end.
%%
%% add_rosteritem(LU, LS, User, Server, Nick, Group, Subscription, Xattrs) ->
%%     subscribe(LU, LS, User, Server, Nick, Group, Subscription, Xattrs).
%%
%% subscribe(LU, LS, User, Server, Nick, Group, Subscription, _Xattrs) ->
%%     ItemEl = build_roster_item(User, Server, {add, Nick, Subscription, Group}),
%%     mod_roster:set_items(
%% 	LU, LS,
%% 	{xmlel, <<"query">>,
%%             [{<<"xmlns">>, <<"jabber:iq:roster">>}],
%%             [ItemEl]}).
%%
%% delete_rosteritem(LocalUser, LocalServer, User, Server) ->
%%     case unsubscribe(LocalUser, LocalServer, User, Server) of
%% 	{atomic, ok} ->
%% 	    push_roster_item(LocalUser, LocalServer, User, Server, remove),
%% 	    ok;
%% 	_  ->
%% 	    error
%%     end.
%%
%% unsubscribe(LU, LS, User, Server) ->
%%     ItemEl = build_roster_item(User, Server, remove),
%%     mod_roster:set_items(
%% 	LU, LS,
%% 	{xmlel, <<"query">>,
%%             [{<<"xmlns">>, <<"jabber:iq:roster">>}],
%%             [ItemEl]}).
%%
%% %% @spec(LU, LS, U, S, Action) -> ok
%% %%       Action = {add, Nick, Subs, Group} | remove
%% %% @doc Push to the roster of account LU@LS the contact U@S.
%% %% The specific action to perform is defined in Action.
%% push_roster_item(LU, LS, U, S, Action) ->
%%     lists:foreach(fun(R) ->
%% 			  push_roster_item(LU, LS, R, U, S, Action)
%% 		  end, ejabberd_sm:get_user_resources(LU, LS)).
%%
%% push_roster_item(LU, LS, R, U, S, Action) ->
%%     LJID = jlib:make_jid(LU, LS, R),
%%     BroadcastEl = build_broadcast(U, S, Action),
%%     ejabberd_router:route(LJID, LJID, BroadcastEl),
%%     Item = build_roster_item(U, S, Action),
%%     ResIQ = build_iq_roster_push(Item),
%%     ejabberd_router:route(LJID, LJID, ResIQ).
%%
%% build_roster_item(U, S, {add, Nick, Subs, Group}) ->
%%     {xmlel, <<"item">>,
%%      [{<<"jid">>, jlib:jid_to_string(jlib:make_jid(U, S, <<>>))},
%%       {<<"name">>, Nick},
%%       {<<"subscription">>, Subs}],
%%      [{xmlel, <<"group">>, [], [{xmlcdata, Group}]}]
%%     };
%% build_roster_item(U, S, remove) ->
%%     {xmlel, <<"item">>,
%%      [{<<"jid">>, jlib:jid_to_string(jlib:make_jid(U, S, <<>>))},
%%       {<<"subscription">>, <<"remove">>}],
%%      []
%%     }.
