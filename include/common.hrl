%%%-------------------------------------------------------------------
%%% @author xin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 三月 2015 下午6:20
%%%-------------------------------------------------------------------
-author("xin").


-define(DEBUG(Message), common:debug(Message)).

-define(IsDEBUG, true).

-record(request, {req, uid, platid=0, token, ip, port, code=0, data="", res=[], ud=[], tlog_params=[], oprdno=undefined, cid=0}).

-define(DEF_PORT,    2222).

