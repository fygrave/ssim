
-module(ssim_parser).

-define(TIMEZONE, 8).

-export[ message_to_struct/2, get_date_now/0, get_date/2, parse_priority/1].

get_date_now() ->
    get_date(int, ssim_util:unix_timestamp(now())).

get_date(str, Date) ->
    {Int, _} = string:to_integer(Date),
    get_date(int, Int);
get_date(int, Int) ->
    NDate =  calendar:gregorian_seconds_to_datetime(Int +
							calendar:datetime_to_gregorian_seconds({{1970, 1,1}, {0,0,0}})),
    {{Year, Month, Day}, {Hour, Min, Sec}} = NDate,
    SDate =  io_lib:fwrite("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0bZ", [Year, Month, Day, Hour, Min, Sec]),
    list_to_binary(SDate).

message_to_struct(Message, Sensor) ->
    SrcIP = parse_src(Message),
    SrcPort = parse_srcport(Message),
    DstPort = parse_dstport(Message),
    Class = parse_class(Message),
    Severity = parse_severity(Message),
    Priority = parse_priority(Message),
    ParserID = parse_plugin_id(Message),
    ParserSID = parse_plugin_sid(Message),
    NewSensor = parse_sensor(Message, Sensor),
    DstIP = parse_dst(Message, binary_to_list(NewSensor)),
    Log = parse_log(Message),
    Date = parse_date(Message),
    Host = parse_host(Message),
    Username = parse_username(Message),
    {ok, {struct, 
	  [
	   {originalMessage, list_to_binary(Message)},
	   {sensor, NewSensor},
	   {src, SrcIP},
	   {dst, DstIP},
	   {class, Class},
	   {severity, Severity},
	   {priority, Priority},
	   {parser_id, ParserID},
	   {parser_sid, ParserSID},
	   {src_port, SrcPort},
	   {dst_port, DstPort},
	   {host, Host},
	   {username, Username},
	   {message, Log},
	   {time, Date}
	  ]
	 }
    }.
      




parse_date(Data) ->
    case parse_regex(Data, "date=\"?(\\d+)\"?") of
	{ok, [ Date, _ ]} ->
	    get_date(str, Date);
	_Else ->
	    get_date(int, ssim_util:unix_timestamp(now()))
    end.



parse_src(Data) ->
    case parse_regex(Data, "src_ip=\"?([^\"]+)\"?") of
	{ok,  [Rez, _ ] } ->
	    list_to_binary(Rez);
	fail ->
	    case parse_regex(Data, "ip_src=\"?([^\"]+)\"?") of
		{ok, [ Rez, _ ]} ->
		    list_to_binary(Rez);
		fail ->
		    null
	    end
    end.


parse_dst(Data, Sensor) ->
    case parse_regex(Data, "dst_ip=\"?([^\"]+)\"?") of
	{ok, [ Rez, _ ]} ->
	    list_to_binary(Rez);
	_Else ->
	    case parse_regex(Data, "ip_dst=\"?([^\"]+)\"?") of
		{ok, [ Rez, _ ]} ->
		    list_to_binary(Rez);
		_Else2 ->
		    list_to_binary(Sensor)
	    end
    end.



parse_srcport(Data) ->
    case parse_regex(Data, "src_port=\"?([^\"]+)\"?") of
	{ok, [ Rez, _ ]} ->
	    {Port, _} = string:to_integer(Rez),
	    Port;
	_Else ->
	    case parse_regex(Data, "tcp_sport=\"?([^\"]+)\"?") of
		{ok, [ Rez, _ ]} ->
		    {Port, _} = string:to_integer(Rez),
		    Port;
		_Else2 ->
		    case parse_regex(Data, "udp_sport=\"?([^\"]+)\"?") of
			{ok, [ Rez, _ ]} ->
			    {Port, _} = string:to_integer(Rez),
			    Port;
			_Else3 ->
			    -1
		    end
	    end
    end.



parse_dstport(Data) ->
    case parse_regex(Data, "dst_port=\"?([^\"]+)\"?") of
	{ok, [ Rez, _ ]} ->
	    {Port, _} = string:to_integer(Rez),
	    Port;
	_Else ->
	    case parse_regex(Data, "tcp_dport=\"?([^\"]+)\"?") of
		{ok, [ Rez, _ ]} ->
		    {Port, _} = string:to_integer(Rez),
		    Port;
		_Else2 ->
		    case parse_regex(Data, "udp_dport=\"?([^\"]+)\"?") of
			{ok, [ Rez, _ ]} ->
			    {Port, _} = string:to_integer(Rez),
			    Port;
			_Else3 ->
			    -1
		    end
	    end
    end.



% we should have full-fledged classifier here later
parse_class(Data) -> 
    case parse_regex(Data, "snort_classification=\"?(\\d+)\"?") of
	{ok, [ Cls, _ ]} ->
	    {Rez, _}= string:to_integer(Cls),
	    Rez;
	_Else ->
	    0
    end.



parse_severity(Data) ->
    case parse_regex(Data, "severity=\"?(\\d+)\"?") of
	{ok, [ Sev, _ ]} ->
	    {Rez, _} = string:to_integer(Sev),
	    Rez;
	_Else ->
	    0
    end.

parse_priority(Data) ->
    case parse_regex(Data, "snort_priority=\"?(\\d+)\"?") of
	{ok, [ Cls, _ ]} ->
	    {Rez, _}  = string:to_integer(Cls),
	    Rez;
	_Else ->
	    0
    end.

parse_plugin_id(Data) ->
    case parse_regex(Data, "plugin_id=\"?(\\d+)\"?") of
	{ok, [ Cls, _ ]} ->
	    {Rez, _} = string:to_integer(Cls),
	    Rez;
	_Else ->
	    0
    end.

parse_plugin_sid(Data) ->
    case parse_regex(Data, "snort_sid=\"?(\\d+)\"?") of
	{ok, [ Cls, _ ]} ->
	    {Rez, _} = string:to_integer(Cls), 
	    Rez;
	_Else ->
	    0
    end.

parse_sensor(Data, Sensor) ->
    case parse_regex(Data, "sensor=\"?([^\"]+)\"?") of
	{ok, [ Cls, _ ]} ->
	    list_to_binary(Cls);
	_Else ->
	    list_to_binary(Sensor)
    end.


parse_username(Data) ->
    case parse_regex(Data, "username=\"?([^\"]+)\"?") of
	{ok, [ Cls, _ ]} ->
	    list_to_binary(Cls);
	_Else ->
	    null
    end.
parse_host(Data) ->
    case parse_regex(Data, "host=\"?([^\"]+)\"?") of
	{ok, [ Cls, _ ]} ->
	    list_to_binary(Cls);
	_Else ->
	    null
    end.


parse_log(Data) ->
    case parse_regex(Data, "log=\"?([^\"]+)\"?") of
	{ok, [ Cls, _ ]} ->
	    list_to_binary(Cls);
	_Else ->
	    case parse_regex(Data, "userdata1=\"?([^\"]+)\"?") of
		{ok, [ Cls, _ ]} ->
		    list_to_binary(Cls);
		_Else2 ->
		    null
	    end

    end.


parse_regex(Data, Regex) ->
    case re:run(Data, Regex,  [{capture, [1, 2], list},unicode]) of
	{match, Rez } ->
	    {ok, Rez};
	_Else ->
	    fail
    end.

 
