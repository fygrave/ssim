%%%-------------------------------------------------------------------
%%% @author fygrave <fygrave@o0o.nu>
%%% @copyright (C) 2010,2011, fygrave
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2010 by fygrave <fygrave@o0o.nu>
%%%-------------------------------------------------------------------

-module(ssim_util).

%%API

-export([
	 unix_timestamp/1,
	 get_env/1, 
	 hex/1,
	 get_datekey/0,
	 to_hex/1,
	 decode_compress_string/1,
	 decode_compress_lstring/1
]).


unix_timestamp({MegaSecs, Secs, MicroSecs}=TS) ->    
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).


get_env(Key) ->
        application:get_env(ssim, Key).

hex(N) when N < 10 ->
    $0+N;

hex(N) when N >= 10, N < 16 ->
    $a+(N-10).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.
    
to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
 
list_to_hexstr([]) -> 
    [];
list_to_hexstr([H|T]) ->
    to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].


decode_compress_string(Data) ->
    zlib:uncompress( hexstr_to_bin(binary_to_list(Data))).

decode_compress_lstring(Data) ->
    zlib:uncompress( hexstr_to_bin(Data)).
   
get_datekey() ->
    {{Yer,Mon,Day},{Hour,Month,Sec}} = calendar:local_time(),

        list_to_binary(io_lib:fwrite("~4..0b-~2..0b-~2..0b-~2..0b-~2..0b-~2..0b-000", [ Yer, Mon, Day, Hour, Month, Sec])).

