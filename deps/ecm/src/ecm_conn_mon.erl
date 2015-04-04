%% @author maomao
%% @doc @todo Add description to ecm_conn_mon.


-module(ecm_conn_mon).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, setMonPool/1]).

-define(CHILD(I, N, P), {N, {I, start_link, [{N, P}]}, temporary, 5000, worker, []}).

-include("ecm_common.hrl").

% 系统状态
-record(ecm_conn_mon, {conns=#{}, host, port, pool=undefined, db_status=0, max_conns=0, conn_sup=undefined, tref=undefined}).

-define(SOCK_OPTION, [binary, {active, false}, {packet, 0}, {nodelay, true}]).

-define(CKTIME, 10000).


%% ====================================================================
%% 公共函数
%% ====================================================================


% 启动监督进程
start_link(Pool) ->
	[PI] = ets:lookup(pools, Pool),
	gen_server:start_link({local, ?PMS(Pool)}, ?MODULE, [PI], []).


% 设定监督的POOL
setMonPool(Pool)->
	gen_server:cast(?MODULE, {set_pool, Pool}).


%% ====================================================================
%% 回调函数
%% ====================================================================


%% init/1
init([PI]) ->
	% {ok,#ecm_conn_mon{}}.
    % 获取连接池的进程
	{ok, TRef} = timer:send_interval(?CKTIME, self(), 'CK_CONN'),
	% 获取连接池的进程
	{ok, #ecm_conn_mon{max_conns=PI#pools.size, pool=PI#pools.pool, db_status=1, conn_sup=PI#pools.conn_sup, host=PI#pools.host, port=PI#pools.port, tref=TRef}}.


%% HANDLE_CALL/3
handle_call(_Request, _From, STATE) ->
    {reply, ok, STATE}.


%% HANDLE_CAST/2
handle_cast(_Msg, STATE) ->
    {noreply, STATE}.


%% HANDLE_INFO/2 || CK_DB_SERVICE_STATUS
% 检查整体环境
handle_info('CK_CONN', State) ->
	% io:format("~n ** CK_CONN:~p || Conns:~p **~n", [State, ets:match_object(State#ecm_conn_mon.pool, #conns{_='_'})]),
	% 获取遍历所有得日志数据
	checkPoolConn(State#ecm_conn_mon.max_conns+1, 1, State#ecm_conn_mon.pool, State#ecm_conn_mon.conn_sup),
	% 结束
	{noreply, State};


%% HANDLE_INFO/2
handle_info(_Info, STATE) ->
    {noreply, STATE}.


%% TERMINATE/2
terminate(_Reason, _STATE) ->
    ok.


%% CODE_CHANGE/3
code_change(_OldVsn, STATE, _Extra) ->
    {ok, STATE}.


%% ====================================================================
%% 内部函数
%% ====================================================================


% 检查日志
checkPoolConn(MaxCount, MaxCount, _, _) ->
	ok;
checkPoolConn(MaxCount, NowCount, Pool, ConnSupervisorPid) ->
	% 获取ID
	[ConnInfo] = ets:lookup(Pool, NowCount),
	% 检查进程是否存货
	case erlang:is_process_alive(ConnInfo#conns.pid) of
		true ->
			ok;
		false ->
			% 删除项
			ets:delete(Pool, NowCount),
			% 启动进程
			{ok, ConnPid} = supervisor:start_child(ConnSupervisorPid, []),
			% 设置启动项
			case ecm:set_pool(ConnPid, Pool) of
				success ->
					ets:insert_new(Pool, #conns{id=NowCount, pid=ConnPid, pool=Pool}),
					ok;
				failed ->
					supervisor:terminate_child(ConnSupervisorPid, ConnPid)
			end
	end,
	checkPoolConn(MaxCount, NowCount+1, Pool, ConnSupervisorPid).


