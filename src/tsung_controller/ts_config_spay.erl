-module(ts_config_spay).

-export([parse_config/2]).


-include("ts_profile.hrl").
-include("ts_spay.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=spay},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->
    Request =
        case ts_config:getAttr(string, Element#xmlElement.attributes, type) of
            "00" ->
                 M = ts_config:getAttr(Element#xmlElement.attributes, machine_id),
                 #spay_request{type="00", machine_id=M};
            "01" ->
                P = ts_config:getAttr(Element#xmlElement.attributes, phone_number),
                Provider = ts_config:getAttr(Element#xmlElement.attributes, provider),
                #spay_request{type="01", phone_number=P, provider=Provider};
            "02" ->
                #spay_request{type="02"}
        end,
    Msg = #ts_request{ack = parse,
                     endpage = true,
                     dynvar_specs  = DynVar,
                     subst   = SubstFlag,
                     match   = MatchRegExp,
                     param   = Request},
    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.