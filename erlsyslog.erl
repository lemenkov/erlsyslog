
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

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include ("erlsyslog.hrl").

start(Args) ->
	gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

init (Args) ->
	process_flag(trap_exit, true),
	case gen_udp:open(0) of
		{ok, Fd} ->
			case Args of
				{Host, Port} ->
					syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("started at [~p:~p]", [Host, Port])),
					{ok, {Fd, Host, Port}};
				_Other ->
					{ok, Host} = inet:gethostname(),
					syslog(Fd, Host, 514, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("started at [~p:~p]", [Host, 514])),
					{ok, {Fd, Host, 514}}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.

handle_call(Message, From, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("Call [~p] from [~p]", [Message, From])),
	{noreply, {Fd, Host, Port}}.

handle_cast({syslog, Who, Facility, Level, Message}, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("Message [~p] from [~p] Facility [~p] at Level [~p]", [Message, Who, Facility, Level])),
	syslog(Fd, Host, Port, Who, Facility, Level, Message),
	{noreply, {Fd, Host, Port}};

handle_cast(Message, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("Other Message [~p]", [Message])),
	{noreply, {Fd, Host, Port}}.

handle_info(Info, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, io_lib:format ("Info [~p]", [Info])),
	{noreply, {Fd, Host, Port}}.

code_change(_OldVsn, {Fd, Host, Port}, _Extra) ->
	syslog(Fd, Host, Port, erlsyslog, ?LOG_CRITICAL, ?FAC_USER, "Code change"),
	{ok, {Fd, Host, Port}}.

terminate(Reason, {Fd, Host, Port}) ->
	syslog(Fd, Host, Port, erlsyslog, erlsyslog:?LOG_CRITICAL, erlsyslog:?FAC_USER, io_lib:format ("terminated due to reason [~w]", [Reason])),
	gen_udp:close(Fd).

syslog(Fd, Host, Port, Who, Facility, Level, Message, Args) ->
	Packet = "<" ++ integer_to_list (Facility bor Level) ++ "> " ++ atom_to_list(Who) ++ ": " ++ Message ++ "\n",
	gen_udp:send(Fd, Host, Port, Packet).

