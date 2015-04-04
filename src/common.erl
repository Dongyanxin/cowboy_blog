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

-export([get_env/1]).

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

  ?DEBUG("~n--------- app init --------~n"),
  common:start_deps(),

  case startupHttpServ() of

    {ok, _HttpServerId} -> db_mysql:init_emysql_pool();

    ERROR ->
      ?DEBUG(ERROR)

  end,

%%   ?DEBUG(db_mysql:emysql_query_user(querty_user_email, #user{email = "xin"})),
  ok.


startupHttpServ() ->

  Dispatch = cowboy_router:compile([
    {"user/_", [
      ?SUBRULE('_', user_http_controller, [])
    ]},
    {'_', [
      ?SUBRULE('_', user_http_controller, [])
    ]}
  ]),

  case cowboy:start_http(http, 100, [{port, 9090}], [
    {env, [{dispatch, Dispatch}]}, {middlewares, [cowboy_router, cowboy_handler]}]) of

    {ok, Pid} -> {ok, Pid};
    ERROR -> ERROR
  end.



get_env(Key) ->

  {ok, Value} = application:get_env(blog, Key),
  Value.




