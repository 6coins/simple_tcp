%%%-------------------------------------------------------------------
%%% @doc
%%% 入口模块
%%% (负责应用)
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server).
-export([start/0,
         stop/0,
         daemon/2]).

-type request() :: binary().                                                % 请求.
-type response() :: binary().                                               % 响应.
-type port_number() :: inet:port_number().                                  % 连接端口.
%-type send_timeout() :: integer() | infinity.                              % 发送超时.
-type start_child_args() :: [start_child_arg()].                            % 启动子进程的参数列表.
-type start_child_arg() ::                                                  % 启动子进程的参数.
    {lsock, lsock()}                                                        % 监听socket.
    | {extra_options, extra_options()}.                                     % 额外选项列表.
-type lsock() :: ssl:sslsocket() | gen_tcp:socket().                        % 监听socket.
-type extra_options() :: [extra_option()].                                  % 额外选项列表.
-type extra_option() ::                                                     % 额外选项.
    {msg_handle, {M :: module(), F :: atom()}}                              % 请求处理函数,默认{tcp_server_util, echo},即原样返回.到时调用如下:Response = M:F(Response),(还有一个完整的解码请求处理请求编码响应的例子见tcp_server_util:msg_handle/1).
    | {connect_type, short | long}                                          % 连接类型,默认short.(长或短连接,服务接收请求发响应后,短连接主动关闭连接,长连接继续等待收请求直到客户端关闭连接他才关闭连接).
    | {is_ssl, boolean()}                                                   % 是否用ssl(即用安全信道传输),默认false
    | {certfile, ssl:path()}                                                % 如果is_ssl是true,用户的证书路径(同ssl:ssloption()中对应项),默认值"certs/server/server.crt"
    | {keyfile, ssl:path()}.                                                % 如果is_ssl是true,用户的私钥路径(同ssl:ssloption()中对应项),默认值"certs/server/server_private_key.pem"

-export_type([request/0, response/0, port_number/0, start_child_args/0]). 

%%--------------------------------------------------------------------
%% @doc
%% 启动应用
%% @end
%%--------------------------------------------------------------------
start() -> application:start(tcp_server).


%%--------------------------------------------------------------------
%% @doc
%% 停止应用
%% @end
%%--------------------------------------------------------------------
stop() -> application:stop(tcp_server).


%%--------------------------------------------------------------------
%% @doc
%% 启动服务,监听指定端口
%% @end
-spec daemon(port_number(), extra_options()) -> ok | {error, Reason :: term()}.
%%--------------------------------------------------------------------
daemon(Port, Extra_options) ->
    case listen(Port, Extra_options) of                                                 % 建立tcp监听
        {ok, LSock} ->                                                                  % 如果建立成功
            case tcp_server_sup:start_child(
                    [{lsock, LSock}, {extra_options, Extra_options}]) of                    % 开启一个子进程(去接受连接)
                {ok, _} -> ok;                                                                  % 开启成功,返回ok
                Error -> {"tcp_server_sup:start_child/1 error", Error}                          % 开启失败,返回错误信息
            end;
        Error ->                                                                        % 如果建立失败
            Error                                                                           % 返回错误信息
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 建立tcp监听
%% @end
-spec listen(port_number(), extra_options()) -> {ok, lsock()} | {error, Reason :: term()}.
%%--------------------------------------------------------------------
listen(Port, Extra_options) ->                                  
    case proplists:get_value(is_ssl, Extra_options, false) of                           % 得到是否用ssl,默认false
        true ->                                                                         % 如果用ssl
            ok = ssl:start(),                                                               % 启动ssl应用
            ssl:listen(Port, get_socketoption() ++ get_ssloption(Extra_options));           % 开启ssl监听
        _ ->                                                                            % 如果不用ssl
            gen_tcp:listen(Port, get_socketoption())                                        % 开启普通监听
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 得到socket相关选项
%% @end
-spec get_socketoption() -> ssl:socketoption().
%%--------------------------------------------------------------------
get_socketoption() -> 
    [binary, {reuseaddr, true}, {active, once}].                              % 监听选项(写死的,注意{active, once}不能改,因为是半异步服务器)


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 得到ssl相关选项
%% @end
-spec get_ssloption(extra_options()) -> ssl:ssloption().
%%--------------------------------------------------------------------
get_ssloption(Extra_options) ->
    [{certfile,           
      proplists:get_value(certfile , Extra_options, "certs/server/server.crt")},            % 证书
     {keyfile,
      proplists:get_value(keyfile, Extra_options, "certs/server/server_private_key.pem")}   % 私钥
     %{reuse_sessions, true}                                                                % 貌似默认有重用会话(貌似是会话恢复) 
    ].


