
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

-include ("erlsyslog.hrl").

%	{ok, Host} = inet:gethostname(),
init ({Port, SyslogHost, SyslogPort}) ->
	case gen_udp:open(Port) of
		{ok, Fd} ->
			syslog(Fd, SyslogHost, SyslogPort, erlsyslog, ?LOG_INFO, ?FAC_USER, io_lib:format ("started", [])),
			{ok, {Fd, SyslogHost, SyslogPort}};
		{error, Reason} ->
			{stop, Reason}
	end.

handle_call(Request, {Fd, Host, Port}) ->
	{ok, {Fd, Host, Port}}.

handle_info(Info, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_INFO, ?FAC_USER, io_lib:format ("Info [~p]", [Info])),
	{ok, {Fd, Host, Port}}.

handle_event({error, SelfPid, {FromPid, Fmt, [Name|Data]}}, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, Name, ?LOG_ERROR, ?FAC_USER, io_lib:format ("~p: " ++ Fmt, [FromPid] ++  Data)),
	{ok, {Fd, Host, Port}};

handle_event({info_msg, SelfPid, {FromPid, Fmt, [Name|Data]}}, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, Name, ?LOG_INFO, ?FAC_USER, io_lib:format ("~p: " ++ Fmt, [FromPid] ++  Data)),
	{ok, {Fd, Host, Port}};

handle_event(Event, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_INFO, ?FAC_USER, io_lib:format ("Info [~p]", [Event])),
	{ok, {Fd, Host, Port}}.

code_change(_OldVsn, {Fd, Host, Port}, _Extra) ->
%	syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, "Code change"),
	{ok, {Fd, Host, Port}}.

terminate(Reason, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("terminated due to reason [~w]", [Reason])),
	gen_udp:close(Fd).

syslog(Fd, Host, Port, Who, Facility, Level, Message) ->
	Packet = "<" ++ integer_to_list (Facility bor Level) ++ "> " ++ atom_to_list(Who) ++ ": " ++ Message ++ "\n",
	gen_udp:send(Fd, Host, Port, Packet).

