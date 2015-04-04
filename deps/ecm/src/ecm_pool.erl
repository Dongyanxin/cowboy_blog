%% @author maomao
%% @doc @todo Add description to ecm_pool.


-module(ecm_pool).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/5, add_pool/4, del_conns/2, get/1, getSupCount/1, killPool/2]).

-define(CHILD(N, M, A, T), {N, {M, start_link, A}, permanent, 5000, T, []}).

-include("ecm_common.hrl").

-record(ecm_pool, {pool=undefined, conns = #{}, host, port=0, count=0, order=0, conn_sup, mon_ref}).

%% ====================================================================
%% 公共函数
%% ====================================================================

% 启动连接池
start_link(Pool, Host, Port, PoolSize, ConnSupPid) ->
	gen_server:start_link({local, Pool}, ?MODULE, [Pool, Host, Port, PoolSize, ConnSupPid], []).


% 添加连接池
add_pool(Pool, Host, Port, PoolSize) ->
	case catch procAddPool(Pool, Host, Port, PoolSize) of
		ok ->
			% 设定池大小的公共量
			ecm_util:set_env(?PSIZE_KEY(Pool), PoolSize);
		{error, ?ERROR_EXIST_SAME_NAME_POOL} ->
			ok;
		{_ErrorType, ErrorMessage} ->
			PoolSup = ?PRS(Pool),
			% 杀死池的根进程
			supervisor:terminate_child(ecm_pools_sup, PoolSup),
			% 移除进程的注册
			supervisor:delete_child(ecm_pools_sup, PoolSup),
			% 检查是否存在该进程数据
			case ets:lookup(pools, Pool) of
				[] ->
					ok;
				[_] ->
					% 检查进程是否还活着
					case ecm_util:process_alive(Pool) of
						true ->
							ok;
						false ->
							% 删除该进程池数据
							ets:delete(pools, Pool)
					end
			end,
			{error, ErrorMessage}	
	end.


% 获取连接池的链接
get(Pool)->
	% 更新随机因子
	random:seed(erlang:now()),
	case ets:lookup(pools, Pool) of
		[PoolInfo] ->
			% io:format("~n ** PoolInfo:~p **~n", [PoolInfo]),
			% 获取随机ID
			ConnID = random:uniform(PoolInfo#pools.size),
			% 获取该进程
			[ConnInfo] = ets:lookup(Pool, ConnID),
			% 检查是否存货
			case erlang:is_process_alive(ConnInfo#conns.pid) of
				true ->
					% 返回进程ID
					ConnInfo#conns.pid;
				false ->
					% 发送检查信息
					% tlog_mon ! check_tlog,
					% 返回进程ID
					ConnInfo#conns.pid
			end;
		_ ->
			{error, no_pool}
	end.


% 删除连接
del_conns(Pool, ID) ->
	gen_server:call(Pool, {remove, ID}, infinity).


% 获取 ecm_conn_sup 的子进程数量
getSupCount(Sup) ->
	% 获取督程的子进程
	SUP_MESSAGE = supervisor:count_children(Sup),
	% 获取工作者数量
	proplists:get_value(workers, SUP_MESSAGE, 0).	


%% ====================================================================
%% 回调函数
%% ====================================================================


%% init/1
init([Pool, Host, Port, PoolSize, ConnSupPid]) ->
	% {ok, #ecm_pool{}}.
	% 获取连接策略
	case initPoolConn(PoolSize+1, 1, Pool, ConnSupPid) of
		ok ->
			{ok, #ecm_pool{pool=Pool, host=Host, port=Port, conn_sup=ConnSupPid}};
		{error, Reason} ->
			?DEBUGX(Reason),
			{stop, Reason}
	end.


%% HANDLE_CALL/3 || GET
handle_call(get, _From, STATE) ->
	% 获取链接池内得链接
	ConnsMap = STATE#ecm_pool.conns,
	% 获取链接的进程
	ConnPid = maps:get(STATE#ecm_pool.order, ConnsMap),
	% 构建新得序号
	ConnOrder = STATE#ecm_pool.order + 1,
	% 检查序号是否超出最大值
	if
		ConnOrder >= STATE#ecm_pool.count ->
			NEW_STATE = STATE#ecm_pool{order=0};
		true ->
			NEW_STATE = STATE#ecm_pool{order=ConnOrder}
	end,
	% 返回数据
    {reply, ConnPid, NEW_STATE};


%% HANDLE_CALL/3
handle_call({remove, ID}, _From, STATE) ->
	NConns = maps:remove(ID, STATE#ecm_pool.conns),
    {reply, ok, STATE#ecm_pool{conns = NConns}};


%% HANDLE_CALL/3
handle_call(_Request, _From, STATE) ->
    {reply, ok, STATE}.


%% HANDLE_CALL/3 || {updateConns, Conns}
handle_cast({updateConns, Conns}, STATE) ->
	% io:format("~n ** NewConnMap:~p **~n", [Conns]),
	F = fun(Conn, ConnMap) ->
			% 获取连接ID和进程
			{Order, NowPid} = Conn,
			% 更新连接映射数据	
			maps:update(Order, NowPid, ConnMap)
		end,
	NewConnMap = lists:foldl(F, STATE#ecm_pool.conns, Conns),
	% io:format("~n ** NewConnMap:~p **~n", [NewConnMap]),
	% 更新新得ConnMap
    {noreply, STATE#ecm_pool{conns=NewConnMap}};


%% HANDLE_CALL/3 || {updateConns, Conns}
handle_cast({rebuildConns, Conns}, STATE) ->
	% io:format("~n ** ReBuildNewConnMap:~p **~n", [STATE#ecm_pool.conns]),
	F = fun(Conn, ConnMap) ->
			% 获取连接ID和进程
			{Order, NowPid} = Conn,
			% 更新连接映射数据	
			maps:update(Order, NowPid, ConnMap)
		end,
	NewConnMap = lists:foldl(F, STATE#ecm_pool.conns, Conns),
	% io:format("~n ** NewConnMap:~p **~n", [NewConnMap]),
	% 更新新得ConnMap
    {noreply, STATE#ecm_pool{conns=NewConnMap}};


%% HANDLE_CAST/2
handle_cast(_Message, STATE) ->
    {noreply, STATE}.


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


% 初始化池连接
initPoolConn(MaxCount, MaxCount, _, _) -> ok;
initPoolConn(MaxCount, NowCount, Pool, PoolConnSupervisorPid) ->
	% ?DEBUGX("~n========================~n"),
	% ?DEBUGX(PoolConnSupervisorPid),
	% 启动进程
	{ok, ConnPid} = supervisor:start_child(PoolConnSupervisorPid, []),
	
	ets:insert_new(Pool, #conns{id=NowCount, pid=ConnPid, pool=Pool}),
	
	% 设定端口号
	%initPoolConn(MaxCount, NowCount+1, Pool, PoolConnSupervisorPid).
	case setConnID(ConnPid, Pool) of
		success ->
			initPoolConn(MaxCount, NowCount+1, Pool, PoolConnSupervisorPid);
		failed ->
			% io:format("~n ** STARTUP TLOG SERV FAILED **~n"),
			% 杀死进程
			kill!ConnPid,
			% 持续初始化
			initPoolConn(MaxCount, NowCount, Pool, PoolConnSupervisorPid)	
	end.


% 设定TLOG端口
setConnID(Pid, Pool) ->
	% 设定端口
	case ecm:set_pool(Pid, Pool) of
		success ->
			success;
		failed ->
			failed
	end.


% 处理添加连接池
procAddPool(Pool, Host, Port, PoolSize)->
	% 检查池是否存在
	checkPoolWhetherExist(Pool),
	% 定义池根督程名称
	PoolRootSupervisorName = ?PRS(Pool),
	% 启动连接池根督程
	{ok, PoolRootSupervisorPid} = ecm_util:procStartupSup(ecm_pools_sup, ?CHILD(PoolRootSupervisorName, ecm_pool_sup, [PoolRootSupervisorName], supervisor), ?ERROR_ADD_POOL_FAILED),
	% 启动连接督程
	{ok, PoolConnSupervisorPid} = ecm_util:procStartupSup(PoolRootSupervisorPid, ?CHILD(undefined,ecm_conn_sup,[],supervisor), ?ERROR_ADD_POOL_FAILED),
	% 初始化连接信息
	ok = initPoolInfo(Pool, Host, Port, PoolSize, PoolConnSupervisorPid),
	% 启动连接池连接
	{ok, _} = ecm_util:procStartupSup(PoolRootSupervisorPid, ?CHILD(Pool, ecm_pool, [Pool,Host,Port,PoolSize,PoolConnSupervisorPid], worker), ?ERROR_INIT_CONNS),
	% 启动监督进程
	{ok, _} = ecm_util:procStartupSup(PoolRootSupervisorName, ?CHILD(?PMS(Pool), ecm_conn_mon, [Pool], worker), ?ERROR_ADD_POOL_FAILED), 
	ok.


% 检查连接池是否存在
checkPoolWhetherExist(Pool) ->
	case ecm_util:process_alive(Pool) of
		true ->
			% 抛错宏2
			?THROW(?ERROR_EXIST_SAME_NAME_POOL);
		false ->
			% 检查表格
			case ets:info(Pool, size) of
				undefined ->
					ok;
				_ ->
					ets:delete(Pool),ok
			end
	end.


% 构建新得连接表
insertPoolData(Pool,Host,Port,PoolSize,ConnSupPid) ->
	case ets:insert_new(pools, #pools{pool=Pool, host=Host, port=Port, size=PoolSize, conn_sup=ConnSupPid}) of
		true ->
			ok;
		false ->
			?THROW(?ERROR_ADD_POOL_FAILED)
	end.


% 初始化连接数据
initPoolInfo(Pool, Host, Port, PoolSize, ConnSupPid) when is_atom(Pool) ->
	% 构建连接池表
	Pool = ets:new(Pool, [public, set, named_table,{keypos, #conns.id},{read_concurrency, true},{write_concurrency, true}]),
	% 将ETS表得所有权转移到池督程
	ets:give_away(Pool, whereis(ecm_pools_sup), []),
	% 将连接池数据写入到池表中
	ok = insertPoolData(Pool, Host, Port, PoolSize, ConnSupPid).


% 杀死连接池
killPool(Ref, Pool) ->
	% 关闭池根督程
	supervisor:terminate_child(ecm_pools_sup, Ref),
	% 移除池根督程
	supervisor:delete_child(ecm_pools_sup, Ref),
	% 移除所有得记录数据
	ets:delete(pools, Pool),
	% 获取所有得连接池链接
	Conns = ets:match_object(conns, #conns{pool=Pool, _='_'}),
	% 清除函数
	F = fun(Conn) -> ets:delete(conns, Conn#conns.pid) end,
	lists:foreach(F, Conns),
	% 执行结束
	ok.


