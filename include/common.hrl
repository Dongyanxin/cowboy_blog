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

-define(GV(Key), common:get_env(Key)).

-define(IsDEBUG, true).

-record(request, {req, uid, platid=0, token, ip, port, code=0, data="", res=[], ud=[], tlog_params=[], oprdno=undefined, cid=0}).


-define(POOL_NAME, blog_pool).
-define(MYSQL_POOL_HOST, mysql_pool_host).
-define(MYSQL_POOL_SIZE, mysql_pool_size).
-define(MYSQL_POOL_PORT, mysql_pool_port).
-define(MYSQL_USER, mysql_user).
-define(MYSQL_PWD, mysql_pwd).
-define(MYSQL_DBNAME, mysql_dbname).


-record(user, {id, username, email, password, date}).
-record(blog_content, {id, content, user_id, title, date}).



-define(PAGESIZE, 10).
