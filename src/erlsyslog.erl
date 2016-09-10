%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2010 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without
%%% restriction, including without limitation the rights to use,
%%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following
%%% conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%----------------------------------------------------------------------

-module(erlsyslog).
-author('lemenkov@gmail.com').

-behaviour(gen_event).

-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include ("../include/erlsyslog.hrl").

init (_) ->
	process_flag(trap_exit, true),
	% Required to read 'erlsyslog' entry from config-file
	application:load(erlsyslog),
	VerbosityLevel = case application:get_env(erlsyslog, verbosity_level) of
		undefined -> priorities(debug);
		{ok, L} -> priorities(L)
	end,
	case erlsyslog_drv:init({"posix call", logopt([pid]), facility(user)}) of
		{ok, Connection} ->
			syslog(Connection, warning, VerbosityLevel, self(), "erlsyslog: started (VerbosityLevel = ~b)", [VerbosityLevel]),
			{ok, {Connection, VerbosityLevel}};
		{stop, Error} ->
			{stop, Error}
	end.


handle_call(Call, _) ->
	error_logger:error_msg("erlsyslog: strange call [~p]", [Call]),
	{remove_handler, {error, {unknown_call, Call}}}.

handle_info({set_verbosity_level, VerbosityLevel}, {Connection, OldVerbosityLevel}) ->
	error_logger:warning_msg("erlsyslog: verbosity changed from ~p to ~p", [OldVerbosityLevel, VerbosityLevel]),
	{ok, {Connection, priorities(VerbosityLevel)}};

handle_info(Info, _) ->
	error_logger:error_msg("erlsyslog: strange info [~p]", [Info]),
	remove_handler.

handle_event({EventLevel, _, {FromPid, Fmt, Data}}, {Connection, VerbosityLevel}) when is_list(Fmt) ->
	syslog(Connection, EventLevel, VerbosityLevel, FromPid, Fmt, Data),
	{ok, {Connection, VerbosityLevel}};

handle_event({ReportLevel, _, {FromPid, _, Report}}, {Connection, VerbosityLevel}) when is_record(Report, report) ->
	syslog(Connection, ReportLevel, VerbosityLevel, FromPid, Report#report.format, Report#report.data),
	{ok, {Connection, VerbosityLevel}};

handle_event({ReportLevel, _, {FromPid, StdType, Report}}, {Connection, VerbosityLevel}) when is_atom(StdType) ->
	syslog(Connection, debug, VerbosityLevel, FromPid, "~p", [Report]),
	{ok, {Connection, VerbosityLevel}};

handle_event(Event, _) ->
	error_logger:error_msg("erlsyslog: strange event [~p]", [Event]),
	remove_handler.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Connection, VerbosityLevel}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	syslog(Connection, warning, VerbosityLevel, self(), "erlsyslog terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]),
	erlsyslog_drv:terminate(Connection).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

syslog(Connection, Priority, VerbosityLevel, FromPid, Fmt, Args) ->
	NumPri = priorities(Priority),
	NumPri =< VerbosityLevel andalso erlsyslog_drv:syslog(Connection, NumPri, FromPid, Fmt, Args).

facility(kern)      -> 0;
facility(user)      -> 8;
facility(mail)      -> 16;
facility(daemon)    -> 24;
facility(auth)      -> 32;
facility(syslog)    -> 40;
facility(lpr)       -> 48;
facility(news)      -> 56;
facility(uucp)      -> 64;
facility(cron)      -> 72;
facility(authpriv)  -> 80;
facility(ftp)       -> 88;
facility(netinfo)   -> 96;
facility(remoteauth)-> 104;
facility(install)   -> 112;
facility(ras)       -> 120;
facility(local0)    -> 16 * 8;
facility(local1)    -> 17 * 8;
facility(local2)    -> 18 * 8;
facility(local3)    -> 19 * 8;
facility(local4)    -> 20 * 8;
facility(local5)    -> 21 * 8;
facility(local6)    -> 22 * 8;
facility(local7)    -> 23 * 8;
facility(N) when is_integer(N) -> N;
facility(_) -> erlang:error(badarg).

openlog_opt(pid)    -> 1;
openlog_opt(cons)   -> 2;
openlog_opt(odelay) -> 4;
openlog_opt(ndelay) -> 8;
openlog_opt(perror) -> 20;
openlog_opt(N) when is_integer(N) -> N;
openlog_opt(_) -> erlang:error(badarg).

logopt([Queue]) -> openlog_opt(Queue);
logopt([Tail|Queue]) ->
    openlog_opt(Tail) bor logopt(Queue);
logopt([]) -> 0;
logopt(N) -> openlog_opt(N).

% error/info/debug_msg
priorities(error) -> 3;
priorities(warning_msg) -> 4;
priorities(info_msg) -> 6;
% error/info/debug_report
priorities(error_report) -> 3;
priorities(warning_report) -> 4;
priorities(info_report) -> 6;

priorities(emerg)   -> 0;
priorities(alert)   -> 1;
priorities(crit)    -> 2;
priorities(err)     -> 3;
priorities(warning) -> 4;
priorities(notice)  -> 5;
priorities(info)    -> 6;
priorities(debug)   -> 7;
priorities(N) when is_integer(N) -> N;
priorities(_) -> erlang:error(badarg).
