-module(test).

-include ("../include/erlsyslog.hrl").

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(ERR1(X,Y), error_logger:error_msg(X, Y)).
-define(INFO1(X,Y), error_logger:info_msg(X, Y)).
-define(WARN1(X,Y), error_logger:warning_msg(X, Y)).

-define(ERR_REPORT(X,Y), error_logger:error_report(#report{name=?MODULE, format=X, data=Y})).
-define(WARN_REPORT(X,Y), error_logger:warning_report(#report{name=?MODULE, format=X, data=Y})).
-define(INFO_REPORT(X,Y), error_logger:info_report(#report{name=?MODULE, format=X, data=Y})).

start(Args) ->
        gen_server:start(?MODULE, Args, []).

start_link(Args) ->
        gen_server:start_link(?MODULE, Args, []).

init (Args) ->
%	error_logger:tty(false),
	error_logger:add_report_handler(erlsyslog, {0, "localhost", 514}),
	{ok, true}.

handle_call(Other, _From, State) ->
%        io:format("Call [~p], State [~p]~n", [Other, State]),
        error_logger:info_msg("Call [~p], State [~p]", [Other, State]),
        {noreply, State}.

handle_cast(Other, State) ->
%        io:format("Cast [~p], State [~p]~n", [Other, State]),
        Ret1 = ?INFO1("IC [~p], State [~p]", [Other, State]),
        Ret2 = ?WARN1("WC [~p], State [~p]", [Other, State]),
        Ret3 = ?ERR1("EC [~p], State [~p]", [Other, State]),
	error_logger:error_report({error, "TEST"}),
	error_logger:warning_report({warning, "TEST"}),
	error_logger:info_report({info, "TEST"}),

	error_logger:error_report(#report{name=?MODULE, format="Just test from ~p: ~p", data=[self(), "TEST"]}),
	error_logger:warning_report(#report{name=?MODULE, format="Just test from ~p: ~p", data=[self(), "TEST"]}),
	error_logger:info_report(#report{name=?MODULE, format="Just test from ~p: ~p", data=[self(), "TEST"]}),

	?ERR_REPORT("Error report! [~p] [~p]", [self(), true]),
	?WARN_REPORT("Warning report! [~p] [~p]", [self(), true]),
	?INFO_REPORT("Info report! [~p] [~p]", [self(), true]),

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

