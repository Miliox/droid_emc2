%%%---------------------------------------------------------
%%% File:      npc.erl
%%% @author:   Emiliano Firmino <emiliano.firmino@gmail.com>
%%% @copyright 2015 Emiliano Firmino
%%% @doc
%%% Net Plunger Test Client
%%% @end
%%% @since 2015 Emiliano Firmino
%%%---------------------------------------------------------
-module(npc).
-author('emiliano.firmino@gmail.com').

-define(HOST, "localhost").
-define(PORT, 1234).
-define(TCP_OPT, [list, {packet, raw}, {active, false}]).

-export([connect/0, send/2, recv/1, close/1]).

connect() ->
    case gen_tcp:connect(?HOST, ?PORT, ?TCP_OPT) of
        {ok, Socket} ->
            {ok,
                {npc, Socket}};
        Error ->
            Error
    end.

send({npc, Socket}, Data) ->
    gen_tcp:send(Socket, Data).

recv({npc, Socket}) ->
    gen_tcp:recv(Socket, 0).

close({npc, Socket}) ->
    gen_tcp:close(Socket).
