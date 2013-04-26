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

-define(DRV_NAME, "erlsyslog_drv").

%% these constants must match those in syslog_drv.c
-define(SYSLOGDRV_OPEN,  1).
-define(SYSLOGDRV_CLOSE, 2).

-include ("../include/erlsyslog.hrl").

init (_) ->
	process_flag(trap_exit, true),
	erl_ddll:start(),
	PrivDir = case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			EbinDir = filename:dirname(code:which(?MODULE)),
			AppPath = filename:dirname(EbinDir),
			filename:join(AppPath, "priv");
		Path ->
			Path
	end,
	LoadResult = case erl_ddll:load_driver(PrivDir, ?DRV_NAME) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, LoadError} ->
			LoadErrorStr = erl_ddll:format_error(LoadError),
			ErrStr = lists:flatten(io_lib:format("could not load driver ~s: ~p", [?DRV_NAME, LoadErrorStr])),
		{stop, ErrStr}
	end,
	case LoadResult of
		ok ->
			Port = erlang:open_port({spawn, ?DRV_NAME}, [binary]),
			Ref = make_ref(),
			Args = term_to_binary({"erlsyslog", 1, 8, term_to_binary(Ref)}),
			try erlang:port_control(Port, ?SYSLOGDRV_OPEN, Args) of
				<<>> ->
					receive
						{Ref, {ok, Log}} ->
							syslog(Log, info, io_lib:format("~p: erlsyslog: started", [self()])),
							{ok, Log};
						{Ref, Result} ->
							{stop, Result}
					end;
				BinError -> {stop, binary_to_term(BinError)}
			catch
				_:Reason -> {stop,  Reason}
			end;
		Error ->
			Error
	end.

handle_call(Call, State) ->
	error_logger:error_msg("erlsyslog: strange call [~p]", [Call]),
	{remove_handler, {error, {unknown_call, Call}}}.

handle_info(Info, Connection) ->
	error_logger:error_msg("erlsyslog: strange info [~p]", [Info]),
	remove_handler.

handle_event({EventLevel, _, {FromPid, Fmt, Data}}, Connection) when is_list(Fmt) ->
	EL = case EventLevel of
		error -> err;
		warning_msg -> warning;
		info_msg -> info
	end,
	syslog(Connection, EL, io_lib:format ("~p: " ++ Fmt, [FromPid | Data])),
	{ok, Connection};

handle_event({ReportLevel, _, {FromPid, _, Report}}, Connection) when is_record(Report, report) ->
	RL = case ReportLevel of
		error_report -> err;
		warning_report -> warning;
		info_report -> info
	end,
	syslog(Connection, RL, io_lib:format ("~p: " ++ Report#report.format, [FromPid | Report#report.data])),
	{ok, Connection};

handle_event({ReportLevel, _, {FromPid, StdType, Report}}, Connection) when is_atom(StdType) ->
	RL = case ReportLevel of
		error_report -> err;
		warning_report -> warning;
		info_report -> info
	end,
	syslog(Connection, RL, io_lib:format ("~p: ~p", [FromPid, Report])),
	{ok, Connection};

handle_event(Event, Connection) ->
	error_logger:error_msg("erlsyslog: strange event [~p]", [Event]),
	remove_handler.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, Connection) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	syslog(Connection, warning, io_lib:format("erlsyslog terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes])),
	try erlang:port_call(Connection, ?SYSLOGDRV_CLOSE, <<>>) of
		Result ->
			Result
	after
		erlang:port_close(Connection)
	end.

syslog(Connection, Priority, Msg) ->
	NumPri = priorities(Priority),
	erlang:port_command(Connection, [<<NumPri:32/big>>, Msg, <<0:8>>]).

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
