-module(ts_spay).
-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_spay.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/2,
         session_defaults/0,
         parse/2,
         parse_bidi/2,
         dump/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session
%% Returns: {ok, ack_type = parse|no_ack|local, persistent = true|false} 
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%% @spec decode_buffer(Buffer::binary(),Session::record(pgsql)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#spay{}) ->
    Buffer. % nothing to do for pgsql

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #spay{}.

%%----------------------------------------------------------------------
%% Function: get_message/2
%% Purpose: Build a message/request ,
%% Args:    record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#spay_request{type="00",machine_id=M},#state_rcv{session=S}) ->
    Out = list_to_binary(["%00,ver01=", M, "1#"]),
    io:format("out ~p~n", [Out]),
    {Out, S};
get_message(#spay_request{type="01",phone_number=P,provider=Provider},#state_rcv{session=S}) ->
    Out = list_to_binary(["%01", Provider, "=", P, "#"]),
    io:format("out ~p~n", [Out]),
    {Out, S};
get_message(#spay_request{type="02"}, #state_rcv{session=S, dyndata=#dyndata{dynvars=DynVars}}) ->
    {ok, PhoneNumber} = ts_dynvars:lookup(phone_number, DynVars),
    {ok, Seq} = ts_dynvars:lookup(seq, DynVars),
    Out = list_to_binary(["%02=10,2,12,", PhoneNumber, ",", Seq, "#"]),
    {Out, S}.



%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:    Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    io:format("closed -----~n"),
    {State#state_rcv{ack_done = true, datasize=0}, [], true};
%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{acc = [], datasize= 0}) ->
    io:format("new resposne~n", []),
    parse(Data, State#state_rcv{datasize= size(Data)});
%% we don't actually do anything
parse(Data, State=#state_rcv{acc = [], dyndata=DynData}) ->
    ?LOGF("~p:parse(~p, #state_rcv{acc=[], dyndata=~p})~n",[?MODULE, Data, DynData],?NOTICE),
    io:format("get ~p~n", [Data]),
    Seq = parseSeq(Data),
    NewDynData = setSeq(Seq, DynData),
    io:format("seq = ~p~n", [Seq]),
    case size(Data) of
    1 ->
        ts_mon:add({count, one_byte});
    _ ->
        ts_mon:add({count, multi_bytes})
    end,
    {State#state_rcv{ack_done = true,dyndata = NewDynData},[],false};
%% more data, add this to accumulator and parse, update datasize
parse(Data, State=#state_rcv{acc=Acc, datasize=DataSize}) ->
    io:format("still get data~n"),
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary,Data/binary >>, State#state_rcv{acc=[], datasize=NewSize}).


setSeq(Seq, DynData=#dyndata{dynvars=DynVars}) ->
    case Seq of
        not_found -> DynData;
        _ -> DynData#dyndata{dynvars = ts_dynvars:set(seq, Seq, DynVars)}
    end.

parseSeq(<<$$,$0,$1,Rest/binary>>) ->
    Size = byte_size(Rest) - 3,
    Seq = binary:part(Rest, Size, -20),
    binary_to_list(Seq);
parseSeq(<<$$,$0,$0,Rest/binary>>) ->
    Size = byte_size(Rest) - 1,
    binary_to_list(binary:part(Rest, Size, -20));
parseSeq(_D) ->
    io:format("no seq found ~n"),
    not_found.

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_spay:parse_config(Element, Conf).


%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Returns: #spay_request
%%----------------------------------------------------------------------
add_dynparams(_Subst, DynData, Param, _HostData) ->
    R = subst(Param, DynData#dyndata.dynvars),
    io:format("add dyn ~p~nparams ~p~nR ~p~n", [DynData, Param, R]),
    R.

subst(Req=#spay_request{type=Type, phone_number=PhoneNumber, machine_id=MachineId}, DynData) ->
    case Type of
        "00" ->
            Req#spay_request{
                    machine_id = ts_search:subst(MachineId, DynData)
            };
        "01" ->
            Req#spay_request{
                    phone_number = ts_search:subst(PhoneNumber, DynData)
            };
        _ ->
            Req
    end.

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
    #dyndata{proto=#spay_dyndata{}}.

dump(A,B) ->
    io:format("dump~n", []),
    ts_plugin:dump(A,B).


parse_bidi(Data, State) ->
    io:format('parse_bidi~n', []),
    ts_plugin:parse_bidi(Data, State).