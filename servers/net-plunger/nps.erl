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
    Pid = spawn(fun() -> accept_loop(ListenSocket, 0) end),

    {server, Pid}.

stop({server, Pid}) ->
    Pid ! stop.

accept_loop(ListenSocket, ConnCount) ->
    case gen_tcp:accept(ListenSocket, 500) of
        {ok, Socket} ->
            io:format("New connection ~w (~w) ~n", [Socket, ConnCount + 1]),
            Pid = spawn(fun() -> init_loop(Socket) end),

            io:format("Spawn handler process ~w~n", [Pid]),
            cmd_loop(ListenSocket, ConnCount + 1);

        {error, timeout} ->
            cmd_loop(ListenSocket, ConnCount);

        {error, Reason} ->
            io:format("Server Error ~w~n, Terminate", [Reason]),
            gen_tcp:close(ListenSocket)
    end.

cmd_loop(ListenSocket, ConnCount) ->
    receive
        stop ->
            ExitStatus = gen_tcp:close(ListenSocket),
            io:format("Stopping Server ~w~n", [ExitStatus])
    after 0 ->
            accept_loop(ListenSocket, ConnCount)
    end.

init_loop(ListenSocket) ->
    put(begin_timestamp, now()),
    put(recv, 0),
    put(sent, 0),
    conn_loop(ListenSocket).

conn_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            L = length(Data),

            put(recv, get(recv) + L),
            io:format("Received Packet of ~w bytes~n", [L]),

            gen_tcp:send(Socket, Data),
            put(sent, get(sent) + L),

            conn_loop(Socket);

        Error ->
            io:format("Connection Closed~n"),
            finish_loop(Error)
    end.

finish_loop(Reason) ->
    {BegMegaSec, BegSec, _} = get(begin_timestamp),
    {EndMegaSec, EndSec, _} = now(),

    TotalSec = (EndMegaSec - BegMegaSec) * (1000 * 1000) + (EndSec - BegSec),

    io:format("Finished connection because ~w~n", [Reason]),
    io:format("Elapsed: ~wsec, or ~wmin, ~whr~n",
        [TotalSec, TotalSec/60, TotalSec/3600]),
    io:format("Recv: ~w B, or ~w KB, or ~w MB~n",
        [get(recv), get(recv)/1024, get(recv)/(1024*1024)]),
    io:format("Sent: ~w B, or ~w KB, or ~w MB~n",
        [get(sent), get(sent)/1024, get(sent)/(1024*1024)]).
