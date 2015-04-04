%%%-------------------------------------------------------------------
%%% @author xin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 三月 2015 下午6:21
%%%-------------------------------------------------------------------
-module(common).
-author("xin").

%% API
-export([debug/1, start_deps/0, init_app/0]).

-export([initEmysql/0]).

-export([startupHttpServ/0]).
-include("common.hrl").

-define(SUBRULE(URIPath, Handler, Opts), {URIPath, Handler, Opts}).


% debug点
debug(Message) ->

  case ?IsDEBUG of

    true -> io:format("~n ** DEBUG-INFO:~p **~n", [Message]);

    _ -> ok
  end.

start_deps() ->

  RR = application:start(ranch),
  CPR = application:start(crypto),
  CLR = application:start(cowlib),
  CR = application:start(cowboy),
  EMYSQL = application:start(emysql),

  Lager = lager:start(),

  io:format("~n ** Ranch:~p | Crypto:~p | Cowlib:~p | Cowboy:~p  | EMysql : ~p Lager : ~p **~n", [RR, CPR, CLR, CR, EMYSQL, Lager]).


init_app() ->

  common:start_deps(),
  ?DEBUG("app init"),
  ok.


startupHttpServ() ->

  Dispatch = cowboy_router:compile([
    {'_', [
      ?SUBRULE('_', http_controller, [])
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, 100, [{port, 9090}], [
    {env, [{dispatch, Dispatch}]}, {middlewares, [cowboy_router, cowboy_handler]}
  ]),
  ok.


initEmysql() ->



  ok.
