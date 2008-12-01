-module(test).

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(ERR(X,Y), error_logger:error_msg(X, [?MODULE] ++ Y)).
-define(INFO(X,Y), error_logger:info_msg(X, [?MODULE] ++ Y)).
-define(WARN(X,Y), error_logger:warning_msg(X, [?MODULE] ++ Y)).

start(Args) ->
        gen_server:start(?MODULE, Args, []).

start_link(Args) ->
        gen_server:start_link(?MODULE, Args, []).

init (Args) ->
	error_logger:tty(false),
	error_logger:add_report_handler(erlsyslog, {0, "localhost", 514}),
	{ok, true}.

handle_call(Other, _From, State) ->
%        io:format("Call [~p], State [~p]~n", [Other, State]),
        error_logger:info_msg("Call [~p], State [~p]", [Other, State]),
        {noreply, State}.

handle_cast(Other, State) ->
%        io:format("Cast [~p], State [~p]~n", [Other, State]),
        Ret1 = ?INFO("IC [~p], State [~p]", [Other, State]),
        Ret2 = ?WARN("WC [~p], State [~p]", [Other, State]),
        Ret3 = ?ERR("EC [~p], State [~p]", [Other, State]),
        {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

handle_info(Other, State) ->
        io:format("Other Info [~p], State [~p]~n", [Other, State]),
        {noreply, State}.

terminate(Reason, State) ->
	error_logger:delete_report_handler(erlsyslog),
	error_logger:tty(true),
	ok.

