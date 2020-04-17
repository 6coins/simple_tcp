%%%-------------------------------------------------------------------
%%% @doc
%%% 客户端模块(短连接)
%%% (负责接受请求回复响应)
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_client).



%% API
-export([request/3,
         request/4,
         ssl_request/3,
         ssl_request/4
        ]).


-type connect_result()  ::  {ok, gen_tcp:socket()} | {error, reason()}.     % 连接结果
-type send_recv_result()::  {ok, response()} | {error, reason()}.           % 发送结果
-type request()         ::  string().                                       % 请求
-type response()        ::  string().                                       % 响应
-type reason()          ::  closed | inet:posix().                          % 错误信息
-type adress()          ::  inet:socket_address() | inet:hostname().        % 连接地址
-type port_number()     ::  inet:port_number().                             % 连接端口
-type send_timeout()    ::  integer() | infinity.                           % 发送超时


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% (不用ssl)发送请求收响应
%% (负责向服务器建立连接,发送请求,断开连接)
%% @end
-spec request(adress(), port_number(), request()) -> send_recv_result() | {error, reason()}.
-spec request(adress(), port_number(), send_timeout(), request()) -> send_recv_result() | {error, reason()}.
%%--------------------------------------------------------------------
request(Address, Port, Request) ->
    request(Address, Port, infinity, Request).


request(Address, Port, Send_timeout, Request) ->
    case connect(Address, Port, Send_timeout) of    % 建立连接
        {ok, Socket} ->                             % 如果建立成功 
            send_recv_close(Socket, Request);           % 发送请求,接收响应,并断开连接
        Error ->                                    % 如果建立失败
            Error                                       % 返回错误结果
    end.


%%--------------------------------------------------------------------
%% @doc
%% (用ssl)发送请求收响应
%% (负责向服务器建立连接,发送请求,断开连接)
%% @end
-spec ssl_request(adress(), port_number(), request()) -> send_recv_result() | {error, reason()}.
-spec ssl_request(adress(), port_number(), send_timeout(), request()) -> send_recv_result() | {error, reason()}.
%%--------------------------------------------------------------------
ssl_request(Address, Port, Request) ->
    ssl_request(Address, Port, infinity, Request).


ssl_request(Address, Port, Send_timeout, Request) ->
    case ssl_connect(Address, Port, Send_timeout) of    % 建立连接
        {ok, Socket} ->                                 % 如果建立成功 
            ssl_send_recv_close(Socket, Request);           % 发送请求,接收响应,并断开连接
        Error ->                                        % 如果建立失败
            Error                                           % 返回错误结果
    end.




%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% (不用ssl)建立连接
%% @end
-spec connect(adress(), port_number(), send_timeout()) -> connect_result().
%%--------------------------------------------------------------------
connect(Address, Port, Send_timeout) ->
    Option = [list, {send_timeout, Send_timeout}, {active, false}],             % 连接选项
    gen_tcp:connect(Address, Port, Option).                                     % 建立连接


%%--------------------------------------------------------------------
%% @private
%% @doc
%% (不用ssl)发送请求,接收响应,并断开连接
%% @end
-spec send_recv_close(gen_tcp:socket(), request()) -> send_recv_result().
%%--------------------------------------------------------------------
send_recv_close(Socket, Request) ->
    case gen_tcp:send(Socket,[Request]) of          % 发送请求
        ok ->                                       % 如成功
            Recv_result = gen_tcp:recv(Socket, 0),      % 接收响应
            gen_tcp:close(Socket),                      % 关闭连接
            Recv_result;                                % 返回接受结果
        Error ->                                    % 如失败
            Error                                       % 返回原因
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% (用ssl)建立连接
%% @end
-spec ssl_connect(adress(), port_number(), send_timeout()) -> connect_result().
%%--------------------------------------------------------------------
ssl_connect(Address, Port, Send_timeout) ->
    ssl:start(),                                                            % 开启ssl应用     
    Option = [list, {send_timeout, Send_timeout}, {active, false}],         % 连接选项(除了这些选项ssl默认可以会话恢复)
    ssl:connect(Address, Port, Option).                                     % 建立连接,返回结果


%%--------------------------------------------------------------------
%% @private
%% @doc
%% (用ssl)发送请求,接收响应,并断开连接
%% @end
-spec ssl_send_recv_close(ssl:sslsocket(), request()) -> send_recv_result().
%%--------------------------------------------------------------------
ssl_send_recv_close(Socket, Request) ->
    case ssl:send(Socket, [Request]) of             % 发送请求
        ok ->                                       % 如成功
            Recv_result = ssl:recv(Socket, 0),          % 接收响应
            ssl:close(Socket),                          % 关闭连接
            Recv_result;                                % 返回接受结果
        Error ->                                    % 如失败
            Error                                       % 返回原因
    end.


