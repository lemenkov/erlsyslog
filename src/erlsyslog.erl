
%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 3 of the
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
	end.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(Info, Connection) ->
	syslog(Connection, erlsyslog, ?LOG_INFO, ?FAC_USER, io_lib:format ("Info [~p]", [Info])),
	{ok, Connection}.

handle_event({ReportLevel, _, {FromPid, StdType, Report}}, Connection) when is_record(Report, report), is_atom(StdType) ->
	RL = case {ReportLevel,StdType} of
		{error_report, std_error} -> ?LOG_ERROR;
		{warning_report, std_warning} -> ?LOG_WARNING;
		{info_report, std_info} -> ?LOG_INFO
	end,
	syslog(Connection, Report#report.name, RL, Report#report.facility, io_lib:format ("~p: " ++ Report#report.format, [FromPid|Report#report.data])),
	{ok, Connection};

handle_event({ReportLevel, _, {FromPid, StdType, Report}}, Connection) when is_atom(StdType) ->
	RL = case {ReportLevel,StdType} of
		{error_report, std_error} -> ?LOG_ERROR;
		{error_report, crash_report} -> ?LOG_ERROR;
		{warning_report, std_warning} -> ?LOG_WARNING;
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
