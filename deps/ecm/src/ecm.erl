%% @author maomao
%% @doc @todo Add description to ecm.


-module(ecm).

-behaviour(gen_server).

-export([start_link/0, set_pool/2, get_sock/1]).

-export([
	set/3,
	cas/4,
	add/3,
	get/2,
	gets/2,
	del/2,
	version/1
]).

-export([keep_recv/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(ecm, {host,	port=0, socket=undefined, pool, timeref, time_sign=0}).

-define(RECV_TIMEOUT, 10000).

-define(CONNECT_TIMEOUT, 10000).

-define(REQUEST_TIMEOUT, 10000).

-define(DB_DURATION, 60000).

-define(PACK_LEN, 0).

-define(MAX_REV_TIMES, 10).

-define(SET, <<115, 101, 116>>).

-define(CAS, <<99, 97, 115>>).

-define(ADD, <<97, 100, 100>>).

-define(SET_RS, <<"STORED\r\n">>).

-define(GET, <<103, 101, 116>>).

-define(GETS, <<103, 101, 116, 115>>).

-define(VERSION, <<118, 101, 114, 115, 105, 111, 110, 13, 10>>).

-define(DELETE, <<100, 101, 108, 101, 116, 101>>).

-define(COMPRESSED_LEV, ecm_util:get_env(compressed_level)).

-define(SOCK_OPTION, [binary, {active, false}, {reuseaddr, true}, {packet, line}, {nodelay, true}]).

-include("../include/ecm_common.hrl").


%% ====================================================================
%% 公共函数
%% ====================================================================


% 启动连接池连接
start_link() ->
	gen_server:start_link(?MODULE, [], []).


% 设定目标连接池
set_pool(Pid, Pool) ->
	% 设定连连接的连接池
	gen_server:call(Pid, {set_pool, Pool}).


% 获取SOCK
get_sock(Pid) ->
	gen_server:call(Pid, 'GET_SOCK', ?REQUEST_TIMEOUT).


% 设定键值
set(Pid, Key, Val) ->
	% 执行操作调用
	Reply = gen_server:call(Pid, {set, Key, Val}, infinity),
	% 返回数据
	Reply.


% 添加键值
add(Pid, Key, Val) ->
	% 执行操作调用
	Reply = gen_server:call(Pid, {add, Key, Val}, infinity),
	% 返回数据
	Reply.


% 获取键值
get(Pid, Key) ->
	% 执行操作调用
	Reply = gen_server:call(Pid, {get, Key}, infinity),
	% 返回数据
	Reply.


% 获取多个键值
gets(Pid, Keys) when is_list(Keys) ->
	% 执行操作调用
	Reply = gen_server:call(Pid, {gets, Keys}, infinity),
	% 返回数据
	Reply.


% 并发更新
cas(Pid, CasUniqueID, Key, Val) ->
	% 执行操作调用
	Reply = gen_server:call(Pid, {cas, CasUniqueID, Key, Val}, infinity),
	% 返回数据
	Reply.


% 获取版本号
version(Pid) ->
	% 执行操作调用
	Reply = gen_server:call(Pid, version, infinity),
	% 返回数据
	Reply.


% 删除
del(Pid, Key) ->
	% 执行操作调用
	Reply = gen_server:call(Pid, {del, Key}, infinity),
	% 返回数据
	Reply.


%% ====================================================================
%% 内部回调函数 
%% ====================================================================


%% init/1
init([]) ->
	{ok, #ecm{}}.


%% HANDLE_CAST/2 | 设定连接池
handle_call({set_pool, Pool}, _From, STATE) ->
	% 获取连接池的
	[PoolInfo] = ets:lookup(pools, Pool),
	% 获取HOST
	Host = PoolInfo#pools.host,
	% 获取端口
	Port = PoolInfo#pools.port,
	% 开始连接
	case gen_tcp:connect(Host, Port, ?SOCK_OPTION, ?CONNECT_TIMEOUT) of
		{ok, Socket} ->
			% 更新服务器状态
			{reply, success, STATE#ecm{socket=Socket, host=Host, port=Port, pool=Pool}, ?DB_DURATION};
		{error, _Reason} ->
			{reply, failed, STATE, ?DB_DURATION}
	end;


% GET_SOCK
handle_call('GET_SOCK', _From, STATE) ->
	{reply, STATE#ecm.socket, STATE};


%% HANDLE_CALL/3 || SET KEY VALUE
handle_call({set, Key, Val}, _From, STATE) ->
	% 套接字
	SOCK = STATE#ecm.socket,
	% 池子
	Pool = STATE#ecm.pool,
	% 获取处理的KEY
	BKey = output(Key),
	% 需要存储的数据
	SaveBin = term_to_binary(Val, [{compressed, ?COMPRESSED_LEV}]),
	% 请求包 压缩数据包
	{ok, PACK} = buildProPack('SET', {BKey, SaveBin}),
	% 发送请求
	case opDoSend(SOCK, PACK) of
		ok ->
			% 返回结果
			{reply, getReturn(SOCK, 'SET'), STATE, ?DB_DURATION};
		{error, Reason} ->
			% 返回结果
			{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
	end;


%% HANDLE_CALL/3 || ADD KEY VALUE
handle_call({add, Key, Val}, _From, STATE) ->
	% 套接字
	SOCK = STATE#ecm.socket,
	% 池子
	Pool = STATE#ecm.pool,
	% 获取处理的KEY
	BKey = output(Key),
	% 需要存储的数据
	SaveBin = term_to_binary(Val, [{compressed, ?COMPRESSED_LEV}]),
	% 检查存储值是否是个Binary
	{ok, PACK} = buildProPack('ADD', {BKey, SaveBin}),
	% 发送请求
	case opDoSend(SOCK, PACK) of
		ok ->
			% 返回结果
			{reply, getReturn(SOCK, 'ADD'), STATE, ?DB_DURATION};
		{error, Reason} ->
			% 返回结果
			{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
	end;


%% HANDLE_CALL/3 || SET KEY VALUE
handle_call({get, Key}, _From, STATE) ->
	% 套接字
	SOCK = STATE#ecm.socket,
	% 池子
	Pool = STATE#ecm.pool,
	% 获取处理的KEY
	BKey = output(Key),
	% 请求包 PACK:GET BIN包, KS:键BIN长度
	{ok, PACK} = buildProPack('GET', {BKey}),
	% 发送请求
	case opDoSend(SOCK, PACK) of
		ok ->
			% 接受请求结果
			case keep_recv(SOCK, <<>>) of
				{ok, RecvPack} when is_binary(RecvPack) ->
					RS = procGetPack(RecvPack);
				{error, ERROR} ->
					RS = {error, ERROR, Pool}
			end,
			% 返回结果
			{reply, RS, STATE, ?DB_DURATION};
		{error, Reason} ->
			% 返回结果
			{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
	end;


% HANDLE_CALL/3 || GETS KEYS
handle_call({gets, Keys}, _From, STATE) ->
	% 套接字	
	SOCK = STATE#ecm.socket,
	% 池子
	Pool = STATE#ecm.pool,
	% 检查Keys的大小
	KeyCount = length(Keys), 
	if
		KeyCount > 255 ->
			% 返回错误信息
			{reply, {error, 'MORE_THAN_MAX_KEY_SIZE'}, STATE};
		true ->
			% 处理Keys
			KeysBin = procKeys(Keys, <<"">>),
			% 构建GETS请求包
			{ok, PACK} = buildProPack('GETS', KeysBin),
			% 发送请求
			case opDoSend(SOCK, PACK) of
				ok ->
					% 接受请求结果
					case keep_recv(SOCK, <<>>) of
						{ok, RecvPack} when is_binary(RecvPack) ->
							{reply, procGetsPack(RecvPack, []), STATE, ?DB_DURATION};
						{error, Reason} ->
							{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
					end;
				{error, Reason} ->
					% 返回结果
					{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
			end
	end;


%% HANDLE_CALL/3 || CAS
handle_call({cas, CasUniqueID, Key, Val}, _From, STATE) ->
	% 套接字	
	SOCK = STATE#ecm.socket,
	% 池子
	Pool = STATE#ecm.pool,
	% 获取处理的KEY
	BKey = output(Key),
	% 需要存储的数据
	SaveBin = term_to_binary(Val, [{compressed, ?COMPRESSED_LEV}]),
	% 请求包 压缩数据包
	{ok, PACK} = buildProPack('CAS', {BKey, CasUniqueID, SaveBin}),
	% 发送请求
	case opDoSend(SOCK, PACK) of
		ok ->
			% 返回结果
			{reply, getReturn(SOCK, 'CAS'), STATE, ?DB_DURATION};
		{error, Reason} ->
			% 返回结果
			{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
	end;


%% HANDLE_CALL/3 || VERSION
handle_call(version, _From, STATE) ->
	% 套接字
	SOCK = STATE#ecm.socket,
	% 池子
	Pool = STATE#ecm.pool,
	% 发送请求
	case opDoSend(SOCK, ?VERSION) of
		ok ->
			% 返回结果
			{reply, getReturn(SOCK, 'VERSION'), STATE, ?DB_DURATION};
		{error, Reason} ->
			% 返回结果
			{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
	end;


%% HANDLE_CALL/3 || DELETE
handle_call({del, Key}, _From, STATE) ->
	% 套接字
	SOCK = STATE#ecm.socket,
	% 池子
	Pool = STATE#ecm.pool,
	% KEY的
	BinKey = output(Key),
	% 发送数据
	SendBin = <<?DELETE/binary, 32, BinKey/binary, 32, 48, 13, 10>>,
	% 发送请求
	case opDoSend(SOCK, SendBin) of
		ok ->
			% 返回结果
			{reply, getReturn(SOCK, 'DEL'), STATE, ?DB_DURATION};
		{error, Reason} ->
			% 返回结果
			{reply, {error, Reason, Pool}, STATE, ?DB_DURATION}
	end;


%% HANDLE_CALL/3
handle_call(_Request, _From, STATE) ->
    {reply, ok, STATE, ?DB_DURATION}.


%% HANDLE_CAST/2
handle_cast(_Message, STATE) ->
    {noreply, STATE}.



%% HANDLE_INFO/2 timeout
handle_info(timeout, STATE) ->
	% 检查状态
	case STATE#ecm.socket of
		undefined ->
			ok;
		SOCK when is_port(SOCK) ->
			% 请求包 PACK:GET BIN包, KS:键BIN长度
			{ok, PACK} = buildProPack('GET', {<<"keep_conn">>}),
			% 发送请求
			case opDoSend(SOCK, PACK) of
				ok ->
					% 接受请求结果
					keep_recv(SOCK, <<>>);
				{error, _Reason} ->
					ok
			end
	end,
	{noreply, STATE, ?DB_DURATION};
	
%% HANDLE_INFO/2
handle_info(_Info, STATE) ->
    {noreply, STATE}.


%% TREMINATE/2
terminate(_Reason, _STATE) ->
    ok.


%% CODE_CHANGE/3
code_change(_OldVsn, STATE, _Extra) ->
    {ok, STATE}.


%% ====================================================================
%% 内部函数
%% ====================================================================


% 输出binary类型数据
output(Word)->
	case is_binary(Word) of
		true ->
			Word;
		false ->
			atom_to_binary(Word, utf8)
	end.


% 发送消息处理
opDoSend(SOCK, PACK) ->
	case gen_tcp:send(SOCK, PACK) of
		ok ->
			ok;	
		{error, ERROR} ->
			{ok, S} = file:open("CLOSED_ALL.dat", [append]),
			io:format(S, "ecm:opDoSend:~w--~w~n", [calendar:local_time(), ERROR]),
			{error, ERROR}
	end.


% 处理KEYS
procKeys([], KeysBin) -> <<KeysBin/binary, 13, 10>>;
procKeys(Keys, KeysBin) ->
	% 获取当前的KEY
	[HeadKey|TailKeys] = Keys,
	% 检查类型
	case is_binary(HeadKey) of
		true ->
			KeyBin = HeadKey;
		false ->
			KeyBin = atom_to_binary(HeadKey, utf8)
	end,
	% 组合KEY
	KeysBin2 = <<KeysBin/binary, 32, KeyBin/binary>>,
	% 继续处理
	procKeys(TailKeys, KeysBin2).


% 获取返回
getReturn(SOCK, TYPE) when is_port(SOCK), is_atom(TYPE) ->
	case gen_tcp:recv(SOCK, ?PACK_LEN) of
		{ok, PACK} ->
			case TYPE of
				'SET' ->
					PACK;
				'CAS' ->
					PACK;
				'ADD' ->
					PACK;
				'VERSION' ->
					PACK;
				'DEL' ->
					PACK
			end;
		{error, ERROR} ->
			{error, ERROR}
	end.


% 构建协议包
buildProPack(Type, Content) ->
	case Type of
		'SET' ->
			{Key, Value} = Content,
			% 获取键的长度
			BodySize = integer_to_binary(byte_size(Value)),
			% 构建包
			PACK = <<?SET/binary, 32, Key/binary, 32, 49, 32, 48, 32, BodySize/binary, 13, 10, Value/binary, 13, 10>>,
			% 返回构建的SET包
			{ok, PACK};
		'CAS' ->
			{Key, CasUniqueID, Value} = Content,
			% 获取键的长度
			BodySize = integer_to_binary(byte_size(Value)),
			% 获取CasUniqueID的binary数据
			CasUniqueIDBin = integer_to_binary(CasUniqueID),
			% 构建包
			PACK = <<?CAS/binary, 32, Key/binary, 32, 49, 32, 48, 32, BodySize/binary, 32, CasUniqueIDBin/binary, 13, 10, Value/binary, 13, 10>>,
			{ok, PACK};
		'ADD' ->
			{Key, Value} = Content,
			% 获取键的长度
			BodySize = integer_to_binary(byte_size(Value)),
			% 构建包
			PACK = <<?ADD/binary, 32, Key/binary, 32, 49, 32, 48, 32, BodySize/binary, 13, 10, Value/binary, 13, 10>>,
			% 返回构建的SET包
			{ok, PACK};
		'GET' ->
			{Key} = Content,
			PACK = <<?GET/binary, 32, Key/binary, 13, 10>>,
			{ok, PACK};
		'GETS' ->
			% 构建数据包	
			PACK = <<?GETS/binary, Content/binary>>,
			% 返回数据数据
			{ok, PACK}
	end.


% 处s理GET协议返回包
keep_recv(SOCK, PACK) ->
	case gen_tcp:recv(SOCK, 0, ?RECV_TIMEOUT) of
		{ok, <<"END\r\n">>} ->
			{ok, PACK};
		{ok, PACK2} ->
			keep_recv(SOCK, <<PACK/binary, PACK2/binary>>);
		{error, ERROR} ->
			{error, ERROR}	
	end.


% 获取GET的DATA BLOCK
procGetPack(PACK) ->
	case PACK of
		<<>> ->
			<<>>;
		_ ->
			% 获取头部和消息块
			[_HeadBin, BlockBin] = binary:split(PACK, <<13, 10>>),
			% 获取包体数据
			binary:part(BlockBin, 0, byte_size(BlockBin) - 2)		
	end.


% 处理接受到得数据信息
procGetsPack(<<>>, RS)->
	RS;
procGetsPack(PACK, RS)->
	% 截取新得数据
	case binary:split(PACK, <<13, 10>>) of
		[HeadBin, TailBin] ->
			% 获取包体长度的初始位置
			case string:tokens(binary_to_list(HeadBin), " ") of
				[_, KeyStr, _, BlockSizeStr, CasUniqueIDStr] ->	
					% 获取KEY
					Key = list_to_binary(KeyStr),
					% 获取唯一值
					CaseUniqueID = list_to_integer(CasUniqueIDStr),
					% 获取BLOCK的长度
					BlockSize = list_to_integer(BlockSizeStr) ,	
					% 获取数据块
					DataBlock = binary:part(TailBin, 0, BlockSize),
					% 组装新结构
					RS2 = [{Key, CaseUniqueID, binary_to_term(DataBlock)}|RS], 
					% 获取尾块得数据大小
					TailBlockSize = byte_size(TailBin), 
					% 获取尾数据
					if
						TailBlockSize =:= BlockSize ->
							PACK2 = <<>>;
						true ->
							PACK2 = binary:part(TailBin, BlockSize+2, byte_size(TailBin)-BlockSize-2)
					end,
					% 循环获取
					procGetsPack(PACK2, RS2);
				_ ->
					RS
			end;
		_ ->
			RS
	end.


%% ====================================================================
%% 测试函数
%% ====================================================================
