%%%-------------------------------------------------------------------
%%% @author fygrave <fygrave@gmail.com>
%%% @copyright (C) 2011, root
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2011 by fygrave <fygrave@gmail.com>
%%%-------------------------------------------------------------------
-module(ssim_mq).

%% API
-export([start_link/0, send_message/1, send_agent/1, receive_message/0, receive_agent/0, subscribe_queue/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(SSIM_EXCHANGE, <<"SSIM">>).
-define(SSIM_KEYS, [
                    <<"messages">>,
                    <<"agents">>,
                   
                   ]).
-define(TIMEOUT, 10000).
-include("deps/amqp_client/include/amqp_client.hrl").


-behaviour(gen_server).




-record(state, {
	  channel, 
	  connection, 
	  exchange = ?SSIM_EXCHANGE
}).

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
send_agent(Agent) ->
    gen_server:cast(?MODULE, {send_message_queue, <<"agent">>, Agent}).
receive_agent() ->
    gen_server:call(?MODULE, {receive_message_queue, <<"agent">>}).

send_message(Msg) ->
    gen_server:cast(?MODULE, {send_message_queue, <<"message">>, Msg}).
receive_message() ->
    gen_server:call(?MODULE, {receive_message_queue, <<"message">>}).

subscribe_queue(Queue) ->
    gen_server:call(?MODULE, {subscribe_queue, Queue}).


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
    io:format("SSIM MQ Init proc: ~p~n",[self()]),
    
    {ok, Params} = ssim_util:get_env(mq_connection_params),
   
    AMQParams = #amqp_params_network{
      username          = proplists:get_value(username, Params),
      password          = proplists:get_value(password, Params),
      virtual_host      = proplists:get_value(virtual_host, Params),
      host              = proplists:get_value(host, Params),
%      port              = undefined,
%      channel_max       = 0,
%      frame_max         = 0,
%      heartbeat         = 0,
      ssl_options       = none },

    io:format("Start connection ~n"),

    case amqp_connect_and_get_channel(AMQParams) of
	{ok, {Connection, Channel}} ->
	    io:format("AMQP connection good~n"),
	    amqp_setup_consumer_q(Channel, <<"message">>, ?SSIM_EXCHANGE, <<"message">>, true),
	    amqp_setup_consumer_q(Channel, <<"agent">>, ?SSIM_EXCHANGE, <<"agent">>, true),
	    io:format("AMQP connection done~n"),
	{ok, #state{
	   channel = Channel,
	   connection = Connection
	  }};

	_Else ->
	    io:format("AMQP connection failure~n"),
	    {error, "connection failed"}
		%% wait and reconnect


    end.


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

handle_call({receive_message_queue, Key}, _From, State) ->
    Reply = amqp_receive_message(Key, State),
    {reply, Reply, State};

handle_call({subscribe_queue, Key}, _From, State) ->
    {Pid, Ref} = _From,
    Reply = amqp_subscribe_queue(Key, Pid, State),
    {reply, Reply, State};

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
handle_cast({send_message_queue, Key, Data},  State) ->
    amqp_send_message(Key, Data, State),
    {noreply, State};

handle_cast({#'basic.consume_ok'{}, Tag}, State) ->
    io:format("Got basic consume~n"),
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
handle_info(_Info, State) ->
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
    Connection = _State#state.connection,
    Channel = _State#state.channel,
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
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

amqp_connect_and_get_channel(Params) ->
    %% Start a connection to the server                                                                                               
    {ok, Connection} = amqp_connection:start(Params),                                                                                 
    {ok, Channel} = amqp_connection:open_channel(Connection),
    ExchangeDeclare = #'exchange.declare'{
      exchange = ?SSIM_EXCHANGE,
      type = <<"topic">>,
      durable = true %% config option?
     },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
    {ok, {Connection,Channel}}.

amqp_receive_message(Key, State) ->
    io:format("AMQP:Receving ~s~n", [ Key]),
    Channel =  State#state.channel,
    case amqp_channel:call(Channel,
			   #'basic.get'{queue = Key, no_ack = true}) of
	{#'basic.get_ok'{}, Content} ->
		 #amqp_msg{payload = Payload} = Content,
		 
	   
	    {ok, payload_decode(Payload)};
	{#'basic.get_empty'{}} ->
	    io:format("AMQP queue is empty"),
	    {queue_empty};
	Else ->             {error, Else}
    end.
 
amqp_subscribe_queue(Key, Pid, State) ->
    io:format("AMQP subscribing to the queue ~s ~p~n", [ Key, Pid ]),
    Channel = State#state.channel,
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Key,
                                                     no_ack = true}, Pid).

amqp_send_message(Key, Data, State) ->
    Channel =  State#state.channel,
    Exchange =  State#state.exchange,
    BasicPublish = #'basic.publish'{exchange = Exchange,
				    routing_key = Key},
    
    NewPayload = payload_encode(Data),
   
    Msg = #amqp_msg{
      payload = NewPayload,
      props = #'P_basic'{delivery_mode=2}
     },
    case Result = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = Msg) of
        ok ->
            io:format("."); % TODO: logging
        else ->
            io:format("Error sending message")
    end,
    Result.
 

amqp_setup_consumer_q(Channel, Q, X, Key, Durable) ->

    QueueDeclare = #'queue.declare'{queue=Q, durable=Durable},
    #'queue.declare_ok'{queue = Q,
                        message_count = MessageCount,
                        consumer_count = ConsumerCount}
        = amqp_channel:call(Channel, QueueDeclare),
    
    %log(queue,{Q, {message_count,MessageCount}, {consumer_count,ConsumerCount}}),
    
    QueueBind = #'queue.bind'{queue = Q,
                              exchange = X,
                              routing_key = Key},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind).


payload_decode(Payload) ->
    binary_to_term(Payload).
payload_encode(Payload) ->
    term_to_binary(Payload, [compressed]).
