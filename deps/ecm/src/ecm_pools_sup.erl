%% @author maomao
%% @doc @todo Add description to pools_sup.


-module(ecm_pools_sup).

-behaviour(supervisor).

-export([init/1]).

-export([start_link/0]).

-define(POOL_CHILD(I, Pool), {Pool, {I, start_link, [Pool]}, temporary, 5000, worker, []}).

-include("ecm_common.hrl").

%% ====================================================================
%% 公共函数
%% ====================================================================


% 启动督程
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ====================================================================
%% 回调函数 
%% ====================================================================


%% init/1
init([]) ->
	% 构建连接池表
	pools = ets:new(pools, [public, set, named_table, {keypos, #pools.pool},{read_concurrency, true}]),
	% 设定督程策略
	{ok,{{one_for_one,10,10}, []}}.


%% ====================================================================
%% 私有函数
%% ====================================================================



