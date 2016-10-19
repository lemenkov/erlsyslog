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

-module(erlsyslog_udp).
-author('lemenkov@gmail.com').

-export([init/1]).
-export([syslog/5]).
-export([terminate/1]).

init({{_Ip, _Port} = Address, _Option, Facility}) ->
	{ok, UdpSock} = gen_udp:open(0),
	{ok, {UdpSock, Address, Facility}}.

syslog({UdpSock, {Ip, Port} = _Address, Facility} = _Connection, NumPri, FromPid, Fmt, Args) ->
	gen_udp:send(UdpSock, Ip, Port, [<<"<">>, integer_to_list (Facility bor NumPri), <<">">>, pid_to_list(FromPid), <<": ">>, io_lib:format(Fmt, Args), <<"\n">>]),
	ok.

terminate({UdpSock, _Address, _Facility} = _Connection) ->
	gen_udp:close(UdpSock).
