%%%-------------------------------------------------------------------
%%% @doc
%%% 工具模块
%%% (负责解码请求,把请求数据分发到对应处理模块,编码响应)
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server_util).

-export([get_extra_options_val/3,
         echo/1,
         msg_handle/1,
         test_handle/1
        ]).



%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 根据"键"得到"启动参数"中"额外选项"中的"值"
%% @end
-spec get_extra_options_val(Key, Start_child_args, Default) -> Val when
    Key :: term(),                                                                  %键
    Start_child_args :: tcp_server:start_child_args(),
    Default :: term(),                                                              % 如果没有键对应的值取的默认值
    Val :: term().                                                                  %值
%%--------------------------------------------------------------------
get_extra_options_val(Key, Start_child_args, Default) -> 
    Extra_options = proplists:get_value(extra_options, Start_child_args),           % 得到额外选项
    _Val = proplists:get_value(Key, Extra_options, Default).                        % 根据Key得到Val


%%--------------------------------------------------------------------
%% @doc
%% 处理请求(原样返回请求)
%% @end
-spec echo(Request) -> Request when
    Request ::  binary().   % 请求
%%--------------------------------------------------------------------
echo(Request) -> Request.


%%--------------------------------------------------------------------
%% @doc
%% 处理请求
%% @end
-spec msg_handle(Request) -> Response when
    Request ::  binary(),   % 请求
    Response::  binary().   % 响应
%%--------------------------------------------------------------------
msg_handle(Request) ->
    try msg_handle_generate_exception(Request)                      % 处理请求,生成响应
    catch
        throw:{ErrorCode, ErrorMsg} ->                                  % 如果处理过程中抛出错误码(如thrw({ErrorCode, ErrorMsg}))
            encode_response_by_params({ErrorCode, ErrorMsg, Request});      % 则根据错误码和信息生成响应 
        Error:Info ->                                                   % 如果处理过程发生其他异常
            io:format("error is :~p,~p~n"   
                     ++ "stacktrace is :~p~n"
                     ++ "request is :~p~n", 
                     [Error, Info, erlang:get_stacktrace(), Request]),      % 则模拟日志记录错误
            encode_response_by_params({"9999", "未知错误"})                 % 并生成未知错误响应
    end.


%% 处理请求(其中可以抛错,如thrw({'CODE', CODE}))
msg_handle_generate_exception(Request) ->
    RequestList = decode_request(Request),                              % 解码请求
    ResponseList = dispatch_handle(RequestList),                        % 分发请求得到得到响应
    Response = encode_response(ResponseList),                           % 编码响应
    Response.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 解码请求
%% (收到的请求是二进制的,要把它解码成erlang数据,以便处理)
%% @end
-spec decode_request(Request) -> RequestList when
    Request     ::  binary(),
    RequestList ::  list().
%%--------------------------------------------------------------------
decode_request(_Request) ->
    _RequestList = [].              % IMPLEMENT


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 编码xml
%% (得到的响应是erlang数据,要把它编码成二进制数据,以便发送)
%% @end
-spec encode_response(ResponseList) -> Response when
    ResponseList::  list(),
    Response    ::  binary().
%%--------------------------------------------------------------------
encode_response(_ResponseList) ->
    _Response = <<"response">>.     % IMPLEMENT


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 分发请求得到得到响应
%% @end
-spec dispatch_handle(RequestList) -> ResponseList when
    RequestList ::  list(),
    ResponseList::  list().
%%--------------------------------------------------------------------
dispatch_handle(RequestList) ->
    {Mod, Func} = {tcp_server_util, test_handle},                   % IMPLEMENT 根据请求得到
    ResponseList = Mod:Func(RequestList),                           % 处理请求得到响应
    ResponseList.                                                   % 返回响应

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 请求分发到的处理模块,供dispatch_handle/1使用,以后需要在其他模块实现
%% @end
%%--------------------------------------------------------------------
test_handle(_RequestList) ->
    [].                             % IMPLEMENT


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据参数编码响应
%% @end
-spec encode_response_by_params(Params) -> Response when
    Params  ::  {Code, Detail} | {Code, Detail, Request},   % 响应参数
    Code    ::  string(),   % 状态码
    Detail  ::  string(),   % 错误信息
    Request ::  binary(),   % 请求
    Response::  binary().   % 响应
%%--------------------------------------------------------------------
encode_response_by_params({_Code, _Detail}) ->
    _Response = <<"">>;             % IMPLEMENT

encode_response_by_params({_Code, _Detail, _Request}) -> 
    _Response = <<"">>.             % IMPLEMENT


