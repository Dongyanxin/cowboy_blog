% 连接池表


-record(pools, {pool, host=undefined, port=0, size=0, conn_sup}).

% 连接表
-record(conns, {id, pid, pool}).

% 池结构
-define(STRUC_POOL(Pool,Host,Port,PoolSize,ConnSupPid), #{pool=>Pool, host=>Host, port=>Port, size=>PoolSize, conn_sup=>ConnSupPid}).

% 抛错宏
-define(THROW(ErrorReason), erlang:throw({error, ErrorReason})).

% 错误宏 ================================================= 开始

-define(ERROR_ADD_POOL_FAILED, -200).			% 添加连接池失败

-define(ERROR_EXIST_SAME_NAME_POOL, -201).		% 已启动同名的连接池

-define(ERROR_INIT_POOL_INFO, -202).			% 插入池信息失败

-define(ERROR_INIT_CONNS, -203).				% 启动池连接失败

% 错误宏 ================================================= 结束


% 数据类型 =============================================== 开始

-define(ITL(V), integer_to_list(V)).			% 整形转列表

-define(LTA(V), list_to_atom(V)).				% 列表转原子
	
-define(ATL(V), atom_to_list(V)).				% 原子转列表

% 数据类型 =============================================== 结束


% 池根督程
-define(PRS(Pool), ?LTA(?ATL(Pool)++"_sup")).

% 池连接监督进程
-define(PMS(Pool), ?LTA(?ATL(Pool)++"_mon")).

% 池大小环变
-define(PSIZE_KEY(Pool), ?LTA(?ATL(Pool)++"_size")).

% 调试输出
-define(DEBUGX(N), io:format("~n ** Debug:~p **~n", [N])).









