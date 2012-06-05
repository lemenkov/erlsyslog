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

-export([test/0]).

-include ("../include/erlsyslog.hrl").

init ({Port, SyslogHost, SyslogPort}) ->
	case gen_udp:open(Port) of
		{ok, Fd} ->
			syslog({Fd, SyslogHost, SyslogPort}, erlsyslog, ?LOG_INFO, ?FAC_USER, "started"),
			{ok, {Fd, SyslogHost, SyslogPort}};
		{error, Reason} ->
			{stop, Reason}
	end;
init (_) ->
	% Required to read 'erlsyslog' entry from config-file
	application:load(erlsyslog),
	{ok, {SyslogHost, SyslogPort}} = application:get_env(erlsyslog, syslog_address),
	init({0, SyslogHost, SyslogPort}).

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(Info, Connection) ->
	syslog(Connection, erlsyslog, ?LOG_INFO, ?FAC_USER, io_lib:format ("Info [~p]", [Info])),
	{ok, Connection}.

handle_event({ReportLevel, _, {FromPid, StdType, Report}}, Connection) when is_record(Report, report), is_atom(StdType) ->
	RL = case {ReportLevel,StdType} of
		{error_report, _} -> ?LOG_ERROR;
		{warning_report, _} -> ?LOG_WARNING;
		{info_report, _} -> ?LOG_INFO
	end,
	syslog(Connection, Report#report.name, RL, Report#report.facility, io_lib:format ("~p: " ++ Report#report.format, [FromPid|Report#report.data])),
	{ok, Connection};

handle_event({ReportLevel, _, {FromPid, StdType, Report}}, Connection) when is_atom(StdType) ->
	RL = case {ReportLevel,StdType} of
		{error_report, _} -> ?LOG_ERROR;
		{warning_report, _} -> ?LOG_WARNING;
		{info_report, _} -> ?LOG_INFO
	end,
	syslog(Connection, FromPid, RL, ?FAC_USER, io_lib:format ("~p", [Report])),
	{ok, Connection};

handle_event({EventLevel, _, {FromPid, Fmt, Data}}, Connection) ->
	EL = case EventLevel of
		error -> ?LOG_ERROR;
		warning_msg -> ?LOG_WARNING;
		info_msg -> ?LOG_INFO
	end,
	syslog(Connection, FromPid, EL, ?FAC_USER, io_lib:format (Fmt, Data)),
	{ok, Connection};

handle_event(Event, Connection) ->
	syslog(Connection, erlsyslog, ?LOG_WARNING, ?FAC_USER, io_lib:format ("Unknown event [~p]", [Event])),
	{ok, Connection}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Fd, Host, Port}) ->
	syslog({Fd, Host, Port}, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("terminated due to reason [~w]", [Reason])),
	gen_udp:close(Fd).

syslog({Fd, Host, Port}, Who, Facility, Level, Message) when is_atom(Who) ->
	W = list_to_binary(atom_to_list(Who)),
	M = list_to_binary(Message),
	P = list_to_binary(integer_to_list(Facility bor Level)),
	gen_udp:send(Fd, Host, Port, <<"<", P/binary, "> ", W/binary, ": ", M/binary, "\n">>);

syslog({Fd, Host, Port}, Who, Facility, Level, Message) when is_pid(Who) ->
	W = list_to_binary(pid_to_list(Who)),
	M = list_to_binary(Message),
	P = list_to_binary(integer_to_list(Facility bor Level)),
	gen_udp:send(Fd, Host, Port, <<"<", P/binary, "> ", W/binary, ": ", M/binary, "\n">>).

test() ->
	io:format("Done!~n").
