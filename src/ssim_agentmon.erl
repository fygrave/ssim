%%%-------------------------------------------------------------------
%%% @author fygrave <fygrave@o0o.nu>
%%% @copyright (C) 2010,2011, fygrave
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2010 by fygrave <fygrave@o0o.nu>
%%%-------------------------------------------------------------------
-module(ssim_agentmon).

-behaviour(gen_server).

%% API
-export([start_link/0,
	queue_message_reader/1,
	 queue_agent_reader/1,
	 check_status/0,
	 statistics/0

	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(AGENT_KEY, <<"agent">>).

-record(state, {riaksock,
	       readers
}).

-include("deps/amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_status() ->
    gen_server:call(?MODULE, {check_status}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("Starting SSIM AgentMon~n"),
    {ok, Params} = ssim_util:get_env(riakc_params),
    {ok, SPid} = riakc_pb_socket:start_link(proplists:get_value(db_hostname, Params), 
					   proplists:get_value(db_port, Params)),
    State = #state{riaksock = SPid},
    Rez = ssim_mq:subscribe_queue(?AGENT_KEY),
    io:format("Subscribed ~p~n", [ Rez]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({statistics}, _From, State) ->
    {ok, State};

handle_call({check_status}, _From, State) ->
    {ok, State};

    

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({up, _Node, _Services}, State) ->
    io:format("Cast node up~n"),
    {noreply, State};
handle_cast({#'basic.consume_ok'{}, Tag}, State) ->
    io:format("Got basic consume~n"),
    {noreply, State};
handle_cast({down, _Node}, State)->
    io:format("Cast node down~n"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info ({nodeup, _Node}, State) ->
    io:format("info: Node up~n"),
    {noreply, State};
handle_info({#'basic.deliver'{}, #amqp_msg{payload = Body}},State) ->
    io:format("info: received agent update: ~s~n", [binary_to_term(Body)]),
    {noreply, State};
handle_info ({nodedown, _Node}, State) ->
    io:format("info: Node down~n"),
    {noreply, State};
handle_info({#'basic.consume_ok'{}, Tag}, State)->
    io:format("Basic consume ok ~p~n", [ Tag]),
    {noreply, State};

handle_info(_Info, State) ->
    io:format("Info ~p~n",[_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
queue_message_reader(RiakSock) ->
    io:format("Receiving messages~n"),
    case ssim_mq:receive_message() of
	{ok, Message} ->
	    io:format("MSG Received ~s~n",[Message]);
	{error, Reason} ->
	    %timer:sleep(1000),
	    io:format("Not received ~s~n", Reason);
	_Else ->
	    io:format("AG Else ~p~n", _Else)
    end,
    
%Object = riakc_obj:new(<<"testlog">>, get_datekey(), mochijson2:encode({struct, [{text, list_to_binary(lists:concat(["Agen\t message ",payload_decode(Body)]))}]})),
%riakc_pb_socket:put(Pid, Object),
    queue_message_reader(RiakSock). % loop
    
queue_agent_reader(RiakSock) ->
    io:format("Receiving agent messages~n"),
    case ssim_mq:receive_agent() of
	{ok, Message} ->
	    io:format("Agent Received ~s~n",[Message]);
	{error, Reason} ->
	    %timer:sleep(1000),
	    io:format("AG Not received ~s~n", Reason);
	_Else ->
	    io:format("AG Else ~p~n", _Else)
    end,

    queue_agent_reader(RiakSock).

