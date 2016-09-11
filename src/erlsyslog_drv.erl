%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2010,2016 Peter Lemenkov <lemenkov@gmail.com>
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

-module(erlsyslog_drv).
-author('lemenkov@gmail.com').

-export([init/1]).
-export([syslog/5]).
-export([terminate/1]).

-define(DRV_NAME, "erlsyslog_drv").

%% these constants must match those in syslog_drv.c
-define(SYSLOGDRV_OPEN,  1).
-define(SYSLOGDRV_CLOSE, 2).

init({_Path, Option, Facility}) ->
	_ = erl_ddll:start(), % deprecated. will return {'error',{'already_started','undefined'}} on newer OTP releases
	PrivDir = case code:priv_dir(erlsyslog) of
		{error, bad_name} ->
			EbinDir = filename:dirname(code:which(erlsyslog)),
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
			Args = term_to_binary({"erlsyslog", Option, Facility, term_to_binary(Ref)}),
			try erlang:port_control(Port, ?SYSLOGDRV_OPEN, Args) of
				<<>> ->
					receive
						{Ref, {ok, Connection}} ->
							{ok, Connection};
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

syslog(Connection, NumPri, FromPid, Fmt, Args) ->
	erlang:port_command(Connection, [<<NumPri:32/big>>, io_lib:format("~p: ", [FromPid]), io_lib:format(Fmt, Args), <<0:8>>]),
	ok.

terminate(Connection) ->
	try erlang:port_call(Connection, ?SYSLOGDRV_CLOSE, <<>>) of
		Result ->
			Result
	after
		erlang:port_close(Connection)
	end.
