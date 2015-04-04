-module(blog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, startup/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

startup() ->
  application:start(blog).

start(_StartType, _StartArgs) ->

  common:init_app(),
  blog_sup:start_link().

stop(_State) ->
    ok.
