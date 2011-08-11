%%%-------------------------------------------------------------------
%%% @author fygrave <fygrave@o0o.nu>
%%% @copyright (C) 2011, fyodor yarochkin
%%% @doc
%%%  SSIM Application: OSSIM Server erlang alternative
%%% @end
%%% Created : 16 Mar 2010 by fyodor yarochkin <fygrave@o0o.nu>
%%%----------------------------------------------------------------
-module(ssim_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    
    case ssim_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
	Error -> 
	    Error
    end.

stop(_State) ->
    ok.
