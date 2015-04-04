%%%-------------------------------------------------------------------
%%% @author xin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 四月 2015 下午2:12
%%%-------------------------------------------------------------------
-module(db_mysql).
-author("xin").

-include("common.hrl").
%% API
-export([init_emysql_pool/0]).
-export([
  emysql_insert_user/2,
  emysql_insert_blog/2,
  emysql_query_user/2,
  emysql_query_user/0,
  emysql_query_blog_content/2,
  emysql_query_blog_content/3
]).

init_emysql_pool() ->

  emysql:add_pool(?POOL_NAME, ?GV(?MYSQL_POOL_SIZE), ?GV(?MYSQL_USER), ?GV(?MYSQL_PWD),
    ?GV(?MYSQL_POOL_HOST), ?GV(?MYSQL_POOL_PORT), ?GV(?MYSQL_DBNAME), utf8),
  ok.

emysql_insert_user(insert_user, #user{username = Username, password = Password, email = Email} = _User) ->

  emysql:prepare(insert_user, << "insert into user(username, email, password) values(?, ?, ?)">>),

  case emysql:execute(?POOL_NAME, insert_user, [Username, Email, Password]) of

    {ok_packet, _, _, _, _} -> ?DEBUG(ok);

    {error_packet, _, _, _, _} -> ?DEBUG(failed)

  end.


emysql_insert_blog(insert_blog, #blog_content{title = Title, content = Content, user_id = UserId}) ->

  emysql:prepare(insert_blog, << "insert into user(title, content, user_id) values(?, ?, ?)">>),

  case emysql:execute(?POOL_NAME, insert_blog, [Title, Content, UserId]) of

    {ok_packet, _, _, _, _} -> ?DEBUG(ok);

    {error_packet, _, _, _, _} -> ?DEBUG(failed)

  end.

emysql_query_user() ->

  Results =  emysql:execute(?POOL_NAME, <<"select * from user">>) ,
  emysql:as_record(Results, user, [id, username, email, password, date]).


emysql_query_user(querty_user_email, #user{email = Email}) ->

  emysql:prepare(query_user_email, <<"select * from user where email = ?">>),
  Results = emysql:execute(?POOL_NAME, query_user_email, [Email]),
  emysql:as_record(Results, user, [id, username, email, password, date]);

emysql_query_user(querty_user_id, #user{id = Id}) ->

  emysql:prepare(query_user_id, <<"select * from user where id = ?">>),
  Results = emysql:execute(?POOL_NAME, query_user_id, [Id]),
  emysql:as_record(Results, user, [id, username, email, password, date]).


emysql_query_blog_content(query_blog_id, #blog_content{id = Id} = _Blog_Content) ->
  emysql:prepare(query_blog_id, <<"select * from blog where id = ?">>),
  Results = emysql:execute(?POOL_NAME, query_blog_id, [Id]),
  emysql:as_record(Results, blog_content, [id, content, user_id, title, date]);


emysql_query_blog_content(query_blog_user, #blog_content{} = Blog_content) ->

  emysql_query_blog_content(query_blog_user, Blog_content, 0);

emysql_query_blog_content(query_blog_title, #blog_content{} = Blog_Content) ->

  emysql_query_blog_content(query_blog_title, Blog_Content, 0).

emysql_query_blog_content(query_blog_user, #blog_content{user_id = UserId} = _Blog_content, Page) ->
  emysql:prepare(query_blog_user, <<"select * from blog where user_id = ? limit ?, ?">>),
  Results = emysql:execute(?POOL_NAME, query_blog_user, [UserId, Page * ?PAGESIZE, ?PAGESIZE]),
  emysql:as_record(Results, blog_content, [id, content, user_id, title, date]);

emysql_query_blog_content(query_blog_title, #blog_content{title = Title} = _Blog_Content, Page) ->
  emysql:prepare(query_blog_title, <<"select * from blog where title = ? limit ?, ?">>),
  Results = emysql:execute(?POOL_NAME, query_blog_title, [Title, Page * ?PAGESIZE, ?PAGESIZE]),
  emysql:as_record(Results, blog_content, [id, content, user_id, title, date]).

