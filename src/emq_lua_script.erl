%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 Feng Lee <feng@emqtt.io>. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_lua_script).

-author("Feng Lee <feng@emqtt.io>").

-export([register_on_message_publish/2,     register_on_client_connected/2,
        register_on_client_disconnected/2,  register_on_client_subscribe/2,
        register_on_client_unsubscribe/2,   register_on_message_acked/2,
        register_on_message_delivered/2,    register_on_session_subscribed/2,
        register_on_session_unsubscribed/2, unregister_hooks/1]).

-export([on_message_publish/2,      on_message_delivered/4,
        on_message_acked/4,         on_client_connected/3,
        on_client_subscribe/4,      on_client_unsubscribe/4,
        on_client_disconnected/3,   on_session_subscribed/4,
        on_session_unsubscribed/4]).


-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_protocol.hrl").

-include("emq_lua_hook.hrl").

-define(EMPTY_USERNAME, "").

-define(HOOK_ADD(A, B, C),      emqttd_hooks:add(A, B, C)).
-define(HOOK_DEL(A, B),         emqttd_hooks:delete(A, B)).




register_on_message_publish(ScriptName, LuaState) ->
    ?HOOK_ADD('message.publish', {ScriptName, fun ?MODULE:on_message_publish/2}, [LuaState]).

register_on_message_delivered(ScriptName, LuaState) ->
    ?HOOK_ADD('message.delivered', {ScriptName, fun ?MODULE:on_message_delivered/4}, [LuaState]).

register_on_message_acked(ScriptName, LuaState) ->
    ?HOOK_ADD('message.acked', {ScriptName, fun ?MODULE:on_message_acked/4}, [LuaState]).

register_on_client_connected(ScriptName, LuaState) ->
    ?HOOK_ADD('client.connected', {ScriptName, fun ?MODULE:on_client_connected/3}, [LuaState]).

register_on_client_subscribe(ScriptName, LuaState) ->
    ?HOOK_ADD('client.subscribe', {ScriptName, fun ?MODULE:on_client_subscribe/4}, [LuaState]).

register_on_client_unsubscribe(ScriptName, LuaState) ->
    ?HOOK_ADD('client.unsubscribe', {ScriptName, fun ?MODULE:on_client_unsubscribe/4}, [LuaState]).

register_on_client_disconnected(ScriptName, LuaState) ->
    ?HOOK_ADD('client.disconnected', {ScriptName, fun ?MODULE:on_client_disconnected/3}, [LuaState]).

register_on_session_subscribed(ScriptName, LuaState) ->
    ?HOOK_ADD('session.subscribed', {ScriptName, fun ?MODULE:on_session_subscribed/4}, [LuaState]).

register_on_session_unsubscribed(ScriptName, LuaState) ->
    ?HOOK_ADD('session.unsubscribed', {ScriptName, fun ?MODULE:on_session_unsubscribed/4}, [LuaState]).


unregister_hooks(ScriptName) ->
    ?HOOK_DEL('message.publish',      {ScriptName, fun ?MODULE:on_message_publish/2}),
    ?HOOK_DEL('message.delivered',    {ScriptName, fun ?MODULE:on_message_delivered/4}),
    ?HOOK_DEL('message.acked',         {ScriptName, fun ?MODULE:on_message_acked/4}),
    ?HOOK_DEL('client.connected',     {ScriptName, fun ?MODULE:on_client_connected/3}),
    ?HOOK_DEL('client.subscribe',     {ScriptName, fun ?MODULE:on_client_subscribe/4}),
    ?HOOK_DEL('client.unsubscribe',   {ScriptName, fun ?MODULE:on_client_unsubscribe/4}),
    ?HOOK_DEL('client.disconnected',  {ScriptName, fun ?MODULE:on_client_disconnected/3}),
    ?HOOK_DEL('session.subscribed',   {ScriptName, fun ?MODULE:on_session_subscribed/4}),
    ?HOOK_DEL('session.unsubscribed', {ScriptName, fun ?MODULE:on_session_unsubscribed/4}).


on_client_connected(ReturnCode, #mqtt_client{client_id = ClientId, username = UserName}, LuaState) ->
    ?LOG(debug, "hook client ClientId=~s UserName=~s connected with code ~p~n", [ClientId, UserName, ReturnCode]),
    case catch luerl:call_function([on_client_connected], [ClientId, UserName, ReturnCode], LuaState) of
        {_Result, _St} ->
            ok;
        Other ->
            ?LOG(error, "lua function on_client_connected() caught exception, ~p", [Other]),
            ok
    end.


on_client_disconnected(Error, #mqtt_client{client_id = ClientId, username = UserName}, LuaState) ->
    ?LOG(debug, "hook client ClientId=~s UserName=~s disconnected with ~p~n", [ClientId, UserName, Error]),
    case catch luerl:call_function([on_client_disconnected], [ClientId, UserName, Error], LuaState) of
        {_Result, _St} ->
            ok;
        Other ->
            ?LOG(error, "lua function on_client_disconnected() caught exception, ~p", [Other]),
            ok
    end.


on_client_subscribe(ClientId, Username, TopicTable, LuaState) ->
    NewTopicTable =
        lists:foldr(fun(TopicItem, Acc) ->
                        case on_client_subscribe_single(ClientId, Username, TopicItem, LuaState) of
                            false -> Acc;
                            NewTopicIem -> [NewTopicIem|Acc]
                        end
                    end, [], TopicTable),
    case NewTopicTable of
        [] -> stop;
        Other -> {ok, Other}
    end.

on_client_subscribe_single(_ClientId, _Username, TopicItem = {<<$$, _Rest/binary>>, _Opts}, _LuaState) ->
    %% ignore topics starting with $
    TopicItem;
on_client_subscribe_single(ClientId, Username, TopicItem = {Topic, Opts}, LuaState) ->
    ?LOG(debug, "hook client(~s/~s) will subscribe: ~p~n", [Username, ClientId, Topic]),
    case catch luerl:call_function([on_client_subscribe], [ClientId, Username, Topic], LuaState) of
        {[false], _St} ->
            false;   % cancel this topic's subscription
        {[NewTopic], _St} ->
            ?LOG(debug, "LUA function on_client_subscribe() return ~p", [NewTopic]),
            {NewTopic, Opts};  % modify topic
        Other ->
            ?LOG(error, "lua function on_client_subscribe() caught exception, ~p", [Other]),
            TopicItem
    end.

on_client_unsubscribe(ClientId, Username, TopicTable, LuaState) ->
    NewTopicTable =
        lists:foldr(fun(TopicItem, Acc) ->
                        case on_client_unsubscribe_single(ClientId, Username, TopicItem, LuaState) of
                            false -> Acc;
                            NewTopicIem -> [NewTopicIem|Acc]
                        end
                    end, [], TopicTable),
    case NewTopicTable of
        [] -> stop;
        Other -> {ok, Other}
    end.

on_client_unsubscribe_single(_ClientId, _Username, TopicItem = {<<$$, _Rest/binary>>, _Opts}, _LuaState) ->
    %% ignore topics starting with $
    TopicItem;
on_client_unsubscribe_single(ClientId, Username, TopicItem = {Topic, Opts}, LuaState) ->
    ?LOG(debug, "hook client(~s/~s) unsubscribe ~p~n", [ClientId, Username, Topic]),
    case catch luerl:call_function([on_client_unsubscribe], [ClientId, Username, Topic], LuaState) of
        {[false], _St} ->
            false;   % cancel this topic's unsubscription
        {[NewTopic], _} ->
            ?LOG(debug, "lua function on_client_unsubscribe() return ~p", [NewTopic]),
            {NewTopic, Opts};  % modify topic
        Other ->
            ?LOG(error, "Topic=~p, lua function on_client_unsubscribe() caught exception, ~p", [Topic, Other]),
            TopicItem
    end.

on_session_subscribed(_ClientId, _Username, TopicItem = {<<$$, _Rest/binary>>, _Opts}, _LuaState) ->
    %% ignore topics starting with $
    {ok, TopicItem};
on_session_subscribed(ClientId, Username, TopicItem = {Topic, _Opts}, LuaState) ->
    ?LOG(debug, "hook session(~s/~s) has subscribed: ~p~n", [Username, ClientId, Topic]),
    case catch luerl:call_function([on_session_subscribed], [ClientId, Username, Topic], LuaState) of
        {_Result, _St} ->
            {ok, TopicItem};
        Other ->
            ?LOG(error, "Topic=~p, lua function on_session_subscribed() caught exception, ~p", [Topic, Other]),
            {ok, TopicItem}
    end.

on_session_unsubscribed(_ClientId, _Username, TopicItem = {<<$$, _Rest/binary>>, _Opts}, _LuaState) ->
    %% ignore topics starting with $
    {ok, TopicItem};
on_session_unsubscribed(ClientId, Username, TopicItem = {Topic, _Opts}, LuaState) ->
    ?LOG(debug, "hook session(~s/~s) has unsubscribed ~p~n", [ClientId, Username, Topic]),
    case catch luerl:call_function([on_session_unsubscribed], [ClientId, Username, Topic], LuaState) of
        {_Result, _St} ->
            {ok, TopicItem};
        Other ->
            ?LOG(error, "Topic=~p, lua function on_session_unsubscribed() caught exception, ~p", [Topic, Other]),
            {ok, TopicItem}
    end.


on_message_publish(Message = #mqtt_message{topic = <<$$, _Rest/binary>>}, _LuaState) ->
    %% ignore topics starting with $
    {ok, Message};
on_message_publish(Message = #mqtt_message{from = {ClientId, Username}, qos = Qos,
                                        retain = Retain, topic = Topic, payload = Payload},
                    LuaState) ->
    ?LOG(debug, "hook message publish ~s~n", [emqttd_message:format(Message)]),
    case catch luerl:call_function([on_message_publish], [ClientId, Username, Topic, Payload, Qos, Retain], LuaState) of
        {[false], _St} ->
            {stop, Message};
        {[Newtopic, NewPayload, NewQos, NewRetain], _St} ->
            ?LOG(debug, "lua function on_message_publish() return ~p", [{Newtopic, NewPayload, NewQos, NewRetain}]),
            {ok, Message#mqtt_message{topic = Newtopic, payload = NewPayload,
                                        qos = round(NewQos), retain = to_retain(NewRetain)}};
        Other ->
            ?LOG(error, "Topic=~p, lua function on_message_publish caught exception, ~p", [Topic, Other]),
            {ok, Message}
    end;
on_message_publish(Message = #mqtt_message{from = Internal, qos = Qos,
                                            retain = Retain, topic = Topic, payload = Payload},
                    LuaState) ->
    ?LOG(debug, "hook message publish ~s~n", [emqttd_message:format(Message)]),
    case catch luerl:call_function([on_message_publish], [Internal, ?EMPTY_USERNAME, Topic, Payload, Qos, Retain], LuaState) of
        {[false], _St} ->
            {stop, Message};
        {[Newtopic, NewPayload, NewQos, NewRetain], _St} ->
            ?LOG(debug, "lua function on_message_publish() return ~p", [{Newtopic, NewPayload, NewQos, NewRetain}]),
            {ok, Message#mqtt_message{topic = Newtopic, payload = NewPayload,
                qos = round(NewQos), retain = to_retain(NewRetain)}};
        Other ->
            ?LOG(error, "Topic=~p, lua function on_message_publish caught exception, ~p", [Topic, Other]),
            {ok, Message}
    end.

on_message_delivered(_ClientId, _Username, #mqtt_message{topic = <<$$, _Rest/binary>>}, _LuaState) ->
    %% ignore topics starting with $
    ok;
on_message_delivered(ClientId, Username,
                    Message=#mqtt_message{topic = Topic, payload = Payload, qos = Qos, retain = Retain}, LuaState) ->
    ?LOG(debug, "hook message delivered ~s~n", [emqttd_message:format(Message)]),
    case catch luerl:call_function([on_message_delivered], [ClientId, Username, Topic, Payload, Qos, Retain], LuaState) of
        {_Result,_St} ->
            ok;
        Other ->
            ?LOG(error, "Topic=~p, lua function on_message_delivered() caught exception, ~p", [Topic, Other]),
            ok
    end.

on_message_acked(_ClientId, _Username, #mqtt_message{topic = <<$$, _Rest/binary>>}, _LuaState) ->
    %% ignore topics starting with $
    ok;
on_message_acked(ClientId, Username,
                Message=#mqtt_message{topic = Topic, payload = Payload, qos = Qos, retain = Retain}, LuaState) ->
    ?LOG(debug, "hook message acked ~s~n", [emqttd_message:format(Message)]),
    case catch luerl:call_function([on_message_acked], [ClientId, Username, Topic, Payload, Qos, Retain], LuaState) of
        {_Result,_St} ->
            ok;
        Other ->
            ?LOG(error, "Topic=~p, lua function on_message_acked() caught exception, ~p", [Topic, Other]),
            ok
    end.



to_retain(0) -> false;
to_retain(1) -> true;
to_retain("true") -> true;
to_retain("false") -> false;
to_retain(<<"true">>) -> true;
to_retain(<<"false">>) -> false;
to_retain(true) -> true;
to_retain(false) -> false;
to_retain(Num) when is_float(Num) ->
    case round(Num) of
        0 -> false;
        _ -> true
    end.


