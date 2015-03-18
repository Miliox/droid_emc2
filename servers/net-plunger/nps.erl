%%%---------------------------------------------------------
%%% File:      nps.erl
%%% @author:   Emiliano Firmino <emiliano.firmino@gmail.com>
%%% @copyright 2015 Emiliano Firmino
%%% @doc
%%% Net Plunger Server
%%% @end
%%% @since 2015 Emiliano Firmino
%%%---------------------------------------------------------
-module(nps).
-author('emiliano.firmino@gmail.com').

-define(PORT, 1234).
-define(TCP_OPT, [list, {packet, raw}, {active, false}, {reuseaddr, true}]).

-export([start/0, stop/1]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, ?TCP_OPT),
    Pid = spawn(fun() -> rcv_loop(ListenSocket) end),

    {server, Pid}.

stop({server, Pid}) ->
    Pid ! stop.

rcv_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 500) of
        {ok, Socket} ->
            io:format("New connection ~w~n", [Socket]),
            Pid = spawn(fun() -> conn_loop(Socket) end),

            io:format("Spawn handler process ~w~n", [Pid]),
            cmd_loop(ListenSocket);

        {error, timeout} ->
            cmd_loop(ListenSocket);

        {error, Reason} ->
            io:format("Server Error ~w~n, Terminate", [Reason]),
            gen_tcp:close(ListenSocket)
    end.

cmd_loop(ListenSocket) ->
    receive
        stop ->
            io:format("Stopping Server~n"),
            ExitStatus = gen_tcp:close(ListenSocket),
            io:format("Server stopped: ~w~n", [ExitStatus])

    after 0 ->
            rcv_loop(ListenSocket)
    end.

conn_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received Packet of ~w~n", [length(Data)]),
            gen_tcp:send(Socket, Data),
            conn_loop(Socket);
        Error ->
            io:format("Connection Closed: ~w~n", [Error])
    end.
