
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

-include ("erlsyslog.hrl").

%	{ok, Host} = inet:gethostname(),
init ({Port, SyslogHost, SyslogPort}) ->
	case gen_udp:open(Port) of
		{ok, Fd} ->
			syslog({Fd, SyslogHost, SyslogPort}, erlsyslog, ?LOG_INFO, ?FAC_USER, io_lib:format ("started", [])),
			{ok, {Fd, SyslogHost, SyslogPort}};
		{error, Reason} ->
			{stop, Reason}
	end.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(Info, Connection) ->
	syslog(Connection, erlsyslog, ?LOG_INFO, ?FAC_USER, io_lib:format ("Info [~p]", [Info])),
	{ok, Connection}.

handle_event({error, _, {FromPid, Fmt, Data}}, Connection) ->
	syslog(Connection, FromPid, ?LOG_ERROR, ?FAC_USER, io_lib:format (Fmt, Data)),
	{ok, Connection};

handle_event({warning_msg, _, {FromPid, Fmt, Data}}, Connection) ->
	syslog(Connection, FromPid, ?LOG_WARNING, ?FAC_USER, io_lib:format (Fmt, Data)),
	{ok, Connection};

handle_event({info_msg, _, {FromPid, Fmt, Data}}, Connection) ->
	syslog(Connection, FromPid, ?LOG_INFO, ?FAC_USER, io_lib:format (Fmt, Data)),
	{ok, Connection};

handle_event({error_report, _, {FromPid, std_error, Report}}, Connection) when is_record(Report, report) ->
	syslog(Connection, Report#report.name, ?LOG_ERROR, Report#report.facility, io_lib:format ("~p: " ++ Report#report.format, [FromPid] ++ Report#report.data)),
	{ok, Connection};

handle_event({error_report, _, {FromPid, std_error, Report}}, Connection) ->
	syslog(Connection, FromPid, ?LOG_ERROR, ?FAC_USER, io_lib:format ("~p", [Report])),
	{ok, Connection};

handle_event({warning_report, _, {FromPid, std_warning, Report}}, Connection) when is_record(Report, report) ->
	syslog(Connection, Report#report.name, ?LOG_WARNING, Report#report.facility, io_lib:format ("~p: " ++ Report#report.format, [FromPid] ++ Report#report.data)),
	{ok, Connection};

handle_event({warning_report, _, {FromPid, std_warning, Report}}, Connection) ->
	syslog(Connection, FromPid, ?LOG_WARNING, ?FAC_USER, io_lib:format ("~p", [Report])),
	{ok, Connection};

handle_event({info_report, _, {FromPid, std_info, Report}}, Connection) when is_record(Report, report) ->
	syslog(Connection, Report#report.name, ?LOG_INFO, Report#report.facility, io_lib:format ("~p: " ++ Report#report.format, [FromPid] ++ Report#report.data)),
	{ok, Connection};

handle_event({info_report, _, {FromPid, std_info, Report}}, Connection) ->
	syslog(Connection, FromPid, ?LOG_INFO, ?FAC_USER, io_lib:format ("~p", [Report])),
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
	Packet = "<" ++ integer_to_list (Facility bor Level) ++ "> " ++ atom_to_list(Who) ++ ": " ++ Message ++ "\n",
	gen_udp:send(Fd, Host, Port, Packet);

syslog({Fd, Host, Port}, Who, Facility, Level, Message) when is_pid(Who) ->
	Packet = "<" ++ integer_to_list (Facility bor Level) ++ "> " ++ pid_to_list(Who) ++ ": " ++ Message ++ "\n",
	gen_udp:send(Fd, Host, Port, Packet).

test() ->
	io:format("Done!~n").
