%%%-------------------------------------------------------------------
%%% @author misterion
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2014 5:42 AM
%%%-------------------------------------------------------------------
-module(mod_log_odbc).
-author("vesvalo@mail.ru").

-behaviour(gen_mod).


%% API
-export([start/2,
  stop/1,
  log_packet_send/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(PROCNAME, ?MODULE).

%%join([], _Sep) -> [];
%%join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].


start(Host, Opts) ->
%%  error_logger:info_msg("MOD_LOG_ODBC Module ~p Host ~p Opts ~p",[?MODULE, {host,Host,host}, {opts,Opts,opts}]),
  %%?INFO_MSG("MOD_LOG_ODBC Module ~p Host ~p Opts ~p",[?MODULE, {host,Host,host}, {opts,Opts,opts}]),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_packet_send, 95).
  %%start_vhs(Host, Opts).

stop(Host) ->
  ok.

%%log_packet_receive(_JID, From, To, _Packet) when From#jid.lserver == To#jid.lserver->
%%  error_logger:info_msg("MOD_LOG_ODBC RECEIVED PACKET _JID: ~p~n From: ~p~n To: ~p~n Packet: ~p~n",[_JID, From, To, _Packet]),
%%  ok; % only log at send time if the message is local to the server

log_packet_send(From, To, Packet) ->
%%  error_logger:info_msg("MOD_LOG_ODBC RECEIVED PACKET From: ~p To: ~p Packet: ~p~n",[From, To, Packet]),
  %%?INFO_MSG("MOD_LOG_ODBC RECEIVED PACKET From: ~p To: ~p Packet: ~p~n",[From, To, Packet]),

  Type = xml:get_tag_attr_s(<<"type">>, Packet),
  BodyElem = xml:get_path_s(Packet,[{elem, <<"body">>}]),

  case Packet of
    {xmlel,<<"message">>,_,_} ->
      if (Type /= <<"error">>) and (Type /= <<"groupchat">>) and (Type /= <<"headline">>) and (<<>> /= BodyElem) ->
          %%error_logger:info_msg("MOD_LOG_ODBC PACKETTYPE ~p BodyElem: ~p",[Type,BodyElem]),
        %%?INFO_MSG("MOD_LOG_ODBC PACKETTYPE ~p BodyElem: ~p",[Type,BodyElem]),
        try
          Sender = ejabberd_odbc:escape(From#jid.luser),
          Receiver = ejabberd_odbc:escape(To#jid.luser),
          MsgText = case BodyElem of
            {_,_,_,CData} -> ejabberd_odbc:escape(xml:get_cdata(CData));
            _ -> ejabberd_odbc:escape(xml:element_to_binary(BodyElem))
          end,

          %%?INFO_MSG("MOD_LOG_ODBC Sender ~p Receiver: ~p PacketData ~p",[Sender, Receiver, MsgText]),
          ejabberd_odbc:sql_query(From#jid.lserver,
            [<<"insert into message_log (sender, receiver, msg) values ('">>,
                Sender, <<"', '">>,
                Receiver, <<"', '">>,
                MsgText, <<"');">>]),
          ok
        catch
          _ -> ok
        end;
        true -> ok
      end;
    _-> ok
  end.
