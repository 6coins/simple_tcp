%%%-------------------------------------------------------------------
%%% @doc
%%% 监督进程
%%% (负责开启子进程去做接受连接等操作)
%%% @end
%%%-------------------------------------------------------------------

-module(tcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 开启监督进程
%% @end
-spec start_link() -> supervisor:startlink_ret().
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @doc
%% 开启一个子进程
%% @end
-spec start_child(tcp_server:start_child_args() ) -> supervisor:startchild_ret().
%%--------------------------------------------------------------------
start_child(Start_child_args) ->
    supervisor:start_child(?SERVER, [Start_child_args]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 当监督进程启动时(supervisor:start_link/[2,3]),的回调函数
%% @end
-spec init(Args) -> Result when 
    Args        ::  [gen_tcp:socket()],
    Result      ::  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} 
                    | ignore.
%%--------------------------------------------------------------------
init(_) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,
    Child = {tcp_server_child, {tcp_server_child, start_link, []},
        Restart, Shutdown, Type, [tcp_server_child]},
    {ok, {SupFlags, [Child]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
