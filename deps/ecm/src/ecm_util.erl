%% @author maomao
%% @doc @todo Add description to ecm_util.


-module(ecm_util).

-export([get_env/1, set_env/2, now/0, procStartupSup/3, process_alive/1]).

-export([
	ceil/1,
	to_tuple/1,
	to_integer/1,
	to_list/1,
	to_binary/1,
	to_float/1,
	f2s/1,
	to_atom/1
]).

-include("ecm_common.hrl").

%% ====================================================================
%% 公共函数
%% ====================================================================

% 获取应用环境变量
get_env(Key) ->
	{ok, Value} = application:get_env(ecm, Key), Value.


% 设定环境变量
set_env(Key, Value) ->
	application:set_env(ecm, Key, Value).


% 日期转秒数
now() ->
    calendar:datetime_to_gregorian_seconds({date(),time()}) - 62167219200 - 8 * 3600.


% 督程启动
procStartupSup(Supervisor, ChildSpec, ErrorCode) ->
	case supervisor:start_child(Supervisor, ChildSpec) of
		{ok, Pid} ->
			{ok, Pid};
		{error, ERROR} ->
			io:format("~n ** ERROR:~p **~n", [ERROR]),
			?THROW(ErrorCode)			
	end.

% 检查进程是否还活着
process_alive(Processor) when is_pid(Processor) or is_atom(Processor) ->
	case is_pid(Processor) of
		true ->
			erlang:is_process_alive(Processor);
		false ->
			case whereis(Processor) of
				undefined ->
					false;
				ProcessorPid ->
					erlang:is_process_alive(ProcessorPid)	
			end
	end.


ceil(F) ->
	I = erlang:trunc(F),
	case F - I of
		M when M > 0 ->
			I + 1;
		_ ->
			I
	end.

%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) ->
	Msg;
to_atom(Msg) when is_binary(Msg) ->
	erlang:list_to_atom(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
	erlang:list_to_atom(Msg);
to_atom(Msg) when is_integer(Msg) ->
	erlang:list_to_atom(erlang:integer_to_list(Msg));
to_atom(_Msg) ->
	throw(other_value).

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) ->
	Msg;
to_list(Msg) when is_atom(Msg) ->
	atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) ->
	binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) ->
	integer_to_list(Msg);
to_list(Msg) when is_float(Msg) ->
	f2s(Msg);
to_list(_) ->
	throw(other_value).

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) ->
	Msg;
to_binary(Msg) when is_atom(Msg) ->
	list_to_binary(atom_to_list(Msg));
%%atom_to_binary(Msg, utf8);
to_binary(Msg) when is_list(Msg) ->
	list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
	list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) ->
	list_to_binary(f2s(Msg));
to_binary(_Msg) ->
	throw(other_value).

%% @doc convert other type to float
to_float(Msg)->
	Msg2 = to_list(Msg),
	list_to_float(Msg2).

%% @doc convert other type to integer
-spec to_integer(Msg :: any()) -> integer().
to_integer(Msg) when is_integer(Msg) ->
	Msg;
to_integer(Msg) when is_binary(Msg) ->
	Msg2 = binary_to_list(Msg),
	list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) ->
	list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) ->
	round(Msg);
to_integer(_Msg) ->
	throw(other_value).

%% @doc convert other type to tuple
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.

%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
	integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
	[A] = io_lib:format("~.2f", [F]),
	A.


%% ====================================================================
%% 私有函数
%% ====================================================================


