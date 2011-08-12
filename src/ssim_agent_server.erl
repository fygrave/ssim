%%%-------------------------------------------------------------------
%%% @author fygrave <fygrave@o0o.nu>
%%% @copyright (C) 2011, fygrave
%%% @doc
%%%
%%% @end
%%% Created : 13 Mar 2010 by fygrave <fygrave@o0o.nu>
%%%-------------------------------------------------------------------
-module(ssim_agent_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {port, lsock, targetHost, targetPort}).

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
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

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
init([Port]) ->
    io:format("SSIM Server Started~n"),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true},{packet, line}, {reuseaddr, true}, {buffer, 4048}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

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

handle_cast(stop, State) ->
    {stop, normal, State}.

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
handle_info({tcp, Socket, RawData}, State) ->
    handle_socket (Socket, RawData),
    {noreply, State};
handle_info({tcp_closed, _Socket}, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
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

handle_socket(Socket, RawData) ->
%    io:format("Data: ~s~n", [RawData]),
    try
	{Cmd,  Msg} = parse_message(RawData),
%	io:format("Got '~s' msg ~s~n", [ Cmd, Msg]),
	case Cmd of
	    "event" ->
		store_message(Msg, event);
	    "connect" ->
		io:format("In connect clause~n"),
		{Seq} = register_connect(Msg),
		gen_tcp:send(Socket, io_lib:fwrite("ok id=\"~s\"~n", [ Seq ]));
	    "session-append-plugin" ->
		{Seq} = plugin_append(Msg),
		gen_tcp:send(Socket, io_lib:fwrite("ok id=\"~s\"~n", [ Seq ]));
	    "agent-date" ->
		register_time(Msg);
	    "plugin-process-started" ->
		plugin_handle(Msg, Cmd);
	    "plugin-process-unknown" ->
		plugin_handle(Msg, Cmd);
	    "plugin-enabled" ->
		plugin_handle(Msg, Cmd);
	    "plugin-process-stopped" ->
		plugin_handle(Msg, Cmd);
	    "snort-event" ->
		store_message(Msg, snort)
	end
    catch
	_Class:Err ->
	    io:format("Woops.. Error ~p!~n", [Err])

    end.

parse_message(Data) ->
    {match, [Cmd, Msg]} = re:run(Data, "(\\S+)\\s+(.*)", [{capture, [1, 2], list},unicode]),
    {Cmd, Msg}.

register_connect(Data) ->
    {match, [Seq, Msg]} = re:run(Data, "id=\"?(\\d+)\"\\s+(.*)",  [{capture, [1, 2], list},unicode]),
    io:format("Connected ~s~n", [Msg]),
    ssim_mq:send_agent(io_lib:fwrite("connect ~s", [ Msg ])),
    {Seq}.

plugin_append(Data) ->
    {match, [Seq, Msg]} = re:run(Data, "id=\"?(\\d+)\"\\s+(.*)",  [{capture, [1, 2], list},unicode]),
    ssim_mq:send_agent(io_lib:fwrite("plugin-append ~s", [ Msg ])),
    {Seq}.


store_message(Msg, snort) ->
    {match, [Sensor, Raw]} = re:run(Msg, "sensor=\"(\\S+)\".*gzipdata=\"(\\S+)\".*",  [{capture, [1, 2], list},unicode]),
    Decoded = binary_to_list(ssim_util:decode_compress_lstring(Raw)),
    io:format("Decoded ~s ~s~n", [ Sensor, Decoded]),
    ssim_mq:send_message(io_lib:fwrite("sensor=\"~s\" ~s",[ Sensor,  Decoded])),
    io:format("Stored~n");

store_message(Msg, event) ->
    ssim_mq:send_message(Msg).

plugin_handle(Msg, Cmd) ->
    ssim_mq:send_agent(io_lib:fwrite("~s ~s", [ Cmd, Msg ])).


register_time(Msg)->
    ssim_mq:send_agent(Msg),
    io:format("Time sync ~s~n", [Msg]).
