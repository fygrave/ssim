%%%-------------------------------------------------------------------
%%% @author fygrave <fygrave@o0o.nu>
%%% @copyright (C) 2010,2011, fygrave
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2010 by fygrave <fygrave@o0o.nu>
%%%-------------------------------------------------------------------
-module(ssim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    io:format("Starting SSIM processes:~n"),

    AChild = {ssim_agent_server, {ssim_agent_server, start_link, [4001]},
	      Restart, Shutdown, Type, [ssim_agent_server]},

 
    SsimAmqp = {ssim_mq,
		{ssim_mq, start_link, []},
		permanent, 5000, worker, [ssim_mq]},
 

    SsimCorrelator = { ssim_correlator,
		       {ssim_correlator, start_link, []},
		       permanent, 5000, worker, [ssim_correlator]},
    SsimAgentmon = { ssim_agentmon,
		    {ssim_agentmon, start_link, []},
		    permanent, 5000, worker, [ssim_agentmon]},

    {ok, {SupFlags, [AChild, SsimAmqp, SsimCorrelator, SsimAgentmon]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
