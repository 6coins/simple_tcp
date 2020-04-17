%%%-------------------------------------------------------------------
%%% @doc
%%% 子进程
%%% (负责接受请求回复响应)
%%% @end
%%%-------------------------------------------------------------------

-module(tcp_server_child).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {is_ssl :: boolean(),                                                % 是否用ssl       
                start_child_args :: undefined | tcp_server:start_child_args(),      % 启动子进程参数
                socket :: undefined | gen_tcp:socket()                              % 接受的连接
               }).

-define(TIMEOUT, 1000 * 60 * 10).   % 超时设成10分钟

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 开启服务
%% @end
-spec start_link(Start_child_args) -> Result when
    Start_child_args :: tcp_server:start_child_args(),
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
%%--------------------------------------------------------------------
start_link(Start_child_args) ->
    gen_server:start_link(?MODULE, [Start_child_args], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 初始化服务
%% @end
-spec init(Args) -> Result when
    Args    ::  [tcp_server:start_child_args()],
    Result  ::  {ok, #state{}} | {ok, #state{}, Timeout} | ignore | {stop, Reason},
    Timeout ::  integer() | infinity,
    Reason  ::  term().
%%--------------------------------------------------------------------
init([Start_child_args]) ->
    self() ! accept,                                                                % 通知自己,等待接受连接
    State = #state{is_ssl = tcp_server_util:get_extra_options_val(is_ssl, Start_child_args, false),
                   start_child_args = Start_child_args},                            % 初始化状态
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理call消息
%% (暂时不关注)
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理cast消息
%% (暂时不关注)
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理非call/cast消息
%% (负责处理tcp请求并回复响应)
%% @end
-spec handle_info(Info, State) -> Result when
    Info    ::  timeout | term(),
    State   ::  #state{},
    Result  ::  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State},
    Timeout ::  integer() | infinity,
    Reason  ::  term().
%%--------------------------------------------------------------------


%% 如果不用ssl,等待接受连接处理
handle_info(accept, #state{is_ssl = false, start_child_args = Start_child_args} = State) ->
    {ok, Socket} = gen_tcp:accept(proplists:get_value(lsock, Start_child_args)),                                       % 等待接受普通tcp连接并建立连接
    {ok, _} = tcp_server_sup:start_child(Start_child_args),                         % 让sup创建子进程等待接受下一个连接
    {noreply, State#state{socket=Socket}, ?TIMEOUT};                                % 如果在指定时间内进程没收到消息,则超时处理



%% 如果不用ssl,收到tcp请求处理
handle_info({tcp, Socket, Request}, #state{start_child_args = Start_child_args,
                                           socket = Socket} = State) ->
    io:format("tcp ------------------~p~n",[Request]),
    {M, F} = tcp_server_util:get_extra_options_val(msg_handle, Start_child_args, 
                                                   {tcp_server_util, echo}),        % 得到处理请求的函数
    Response = M:F(Request),                                                        % 处理请求得到响应数据
    send_handle(Socket, Response, State);                                           % 发送响应和之后的处理


%% 如果不用ssl,收到tcp关闭处理
handle_info({tcp_closed, _}, State) ->
    io:format("tcp_closed------------------~n"),
    {stop, normal, State};                                                          % 关闭进程(和连接)


%% 如果不用ssl,收到tcp关闭错误
handle_info({tcp_error, _, Reason}, State) ->
    io:format("tcp_error------------------~n"),
    {stop, Reason, State};                                                          % 关闭进程(和连接)


%% 如果用ssl,等待接受连接处理
handle_info(accept, #state{is_ssl = true, start_child_args = Start_child_args} = State) ->
    {ok, Socket} = ssl:transport_accept(proplists:get_value(lsock, Start_child_args)),                                 % 等待接受普通连接
    ok = ssl:ssl_accept(Socket),                                                    % 完成ssl握手并建立连接
    {ok, _} = tcp_server_sup:start_child(Start_child_args),                         % 让sup创建子进程等待接受下一个连接
    {noreply, State#state{socket=Socket}, ?TIMEOUT};                                % 如果在指定时间内进程没收到消息,则超时处理


%% 如果用ssl,收到tcp请求处理
handle_info({ssl, Socket, Request}, #state{start_child_args = Start_child_args,
                                           socket = Socket} = State) ->
    io:format("ssl ------------------~p~n",[Request]),
    {M, F} = tcp_server_util:get_extra_options_val(msg_handle, Start_child_args, 
                                                   {tcp_server_util, echo}),        % 得到处理请求的函数
    Response = M:F(Request),                                                        % 处理请求得到响应数据
    send_handle(Socket, Response, State);                                           % 发送响应和之后的处理


%% 如果用ssl,收到tcp关闭处理
handle_info({ssl_closed, _}, State) ->
    io:format("ssl_closed------------------~n"),
    {stop, normal, State};                                                          % 关闭进程(和连接)


%% 如果用ssl,收到tcp错误处理
handle_info({ssl_error, _, Reason}, State) ->
    io:format("ssl_error------------------~n"),
    {stop, Reason, State};                                                          % 关闭进程(和连接)


%% 无论是否用ssl,超时处理
handle_info(timeout, State) ->
    {stop, normal, State}.                                                          % 关闭进程(和连接)


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 进程终止回调的函数
%% (负责清理工作)
%% @end
-spec terminate(Reason, State) -> gen_server:void() when
    Reason  :: term(),
    State   ::  #state{}.
%%--------------------------------------------------------------------
terminate(_Reason, #state{is_ssl = false, socket = Socket}) ->      % 如果不用ssl
    case is_port(Socket) of
        true -> gen_tcp:close(Socket);      % 如果有连接则关闭连接
        false -> ok
    end;


terminate(_Reason, #state{is_ssl = true, socket=Socket}) ->         % 如果用ssl
    ssl:close(Socket).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 代码改变时覆盖进程状态
%% (暂时不关注)
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 发送响应和之后的处理
%% @end
%%--------------------------------------------------------------------
-spec send_handle(Socket, Response, State) -> Result when
    Socket :: gen_tcp:socket(), 
    Response :: binary(),
    State :: #state{},
    Result  ::  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State},
    Timeout ::  integer() | infinity,
    Reason  ::  term().
%% @end
%%--------------------------------------------------------------------
send_handle(Socket, Response, #state{start_child_args = Start_child_args} = State) ->
    Connect_type = tcp_server_util:get_extra_options_val(connect_type , Start_child_args, short),
    send_handle(Connect_type, Socket, Response, State).


send_handle(short, Socket, Response, #state{is_ssl = false} = State) ->         % 如果是短连接,且不用ssl
    case gen_tcp:send(Socket, Response) of                                          % 回复响应
        ok ->                                                                       % 如回复成功
            {stop, normal, State};                                                      % 关闭进程(和连接)
        {error, Error} ->                                                           % 如回复失败
            {stop, Error, State}                                                        % 关闭进程(和连接)
    end;


send_handle(long, Socket, Response, #state{is_ssl = false} = State) ->          % 如果是长连接,且不用ssl
    case gen_tcp:send(Socket, Response) of                                          % 回复响应
        ok ->                                                                       % 如回复成功
            ok = inet:setopts(Socket, [{active, once}]),                                % 激活连接继续接收数据
            {noreply, State, ?TIMEOUT};                                                 % 如果在指定时间内进程没收到消息,则超时处理
        {error, Error} ->                                                           % 如回复失败
            {stop, Error, State}                                                        % 关闭进程(和连接)
    end;


send_handle(short, Socket, Response, #state{is_ssl = true} = State) ->         % 如果是短连接,且用ssl
    case ssl:send(Socket, Response) of                                              % 回复响应
        ok ->                                                                       % 如回复成功
            {stop, normal, State};                                                      % 关闭进程(和连接)
        {error, Error} ->                                                           % 如回复失败
            {stop, Error, State}                                                        % 关闭进程(和连接)
    end;

send_handle(long, Socket, Response, #state{is_ssl = true} = State) ->          % 如果是长连接,且用ssl
    case ssl:send(Socket, Response) of                                              % 回复响应
        ok ->                                                                       % 如回复成功
            ok = ssl:setopts(Socket, [{active, once}]),                                % 激活连接继续接收数据
            {noreply, State, ?TIMEOUT};                                                 % 如果在指定时间内进程没收到消息,则超时处理
        {error, Error} ->                                                           % 如回复失败
            {stop, Error, State}                                                        % 关闭进程(和连接)
    end.


