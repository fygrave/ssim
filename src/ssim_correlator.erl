%%%-------------------------------------------------------------------
%%% @author fygrave <fygrave@o0o.nu>
%%% @copyright (C) 2010,2011, fygrave
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2010 by fygrave <fygrave@o0o.nu>
%%%-------------------------------------------------------------------
-module(ssim_correlator).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 check_status/0,
	 statistics/0

	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define (MESSAGE_KEY, <<"message">>).

-record(state, {riaksock,
	       msgcount,
		bucket
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
    io:format("Starting SSIM Correlator~n"),
    {ok, Params} = ssim_util:get_env(riakc_params),
    {ok, SPid} = riakc_pb_socket:start_link(proplists:get_value(db_hostname, Params), 
					   proplists:get_value(db_port, Params)),
    State = #state{riaksock = SPid, msgcount = 0, bucket = proplists:get_value(bucket, Params)},
    Rez = ssim_mq:subscribe_queue(?MESSAGE_KEY),
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
% this really should be moved to ssim_mq
handle_info({#'basic.deliver'{}, #amqp_msg{payload = Body}},State) ->
    store_message(binary_to_term(Body), State#state.riaksock, State#state.bucket),
    NewState = #state{ riaksock = State#state.riaksock,  msgcount = State#state.msgcount + 1},
    {noreply, NewState};
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
store_message(Message, RiakPid, Bucket) ->
    io:format("Receiving message ~p ..",[Message]),
    Object = riakc_obj:new(Bucket, ssim_util:get_datekey(),iolist_to_binary(mochijson2:encode(Message)), <<"application/json">>),
    riakc_pb_socket:put(RiakPid, Object),
    io:format("Message stored ~p~n", [Object]). 

