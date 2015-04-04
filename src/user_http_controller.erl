%%%-------------------------------------------------------------------
%%% @author xin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 四月 2015 下午4:24
%%%-------------------------------------------------------------------
-module(user_http_controller).
-author("xin").

%% API
-export([init/3, handle/2, terminate/3]).
-include("common.hrl").

init(_Transport, Req, []) ->
  {ok, Req, {}}.


% 处理请求
handle(Req, State) ->

  {ok, Req2} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], "Hello world!", Req),
  Req3 = cowboy_req:set_resp_cookie(<<"sessionid">>, <<"adsfasdfas">>, [
    {max_age, 3600}
  ], Req2),

  ?DEBUG(Req3),
  {CookieVal, Req6} = cowboy_req:cookie(<<"sessionid">>, Req),
  ?DEBUG(CookieVal),
  {ok, Req3, State}.


terminate(_Reason, _Req, _State) ->
  ok.