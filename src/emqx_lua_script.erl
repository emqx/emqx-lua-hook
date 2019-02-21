%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_lua_script).

-include("emqx_lua_hook.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").

-export([register_on_message_publish/2, register_on_client_connected/2,
         register_on_client_disconnected/2, register_on_client_subscribe/2,
         register_on_client_unsubscribe/2, register_on_message_acked/2,
         register_on_message_delivered/2, register_on_session_subscribed/2,
         register_on_session_unsubscribed/2, unregister_hooks/1]).

-export([on_message_publish/3, on_message_delivered/4, on_message_acked/4,
         on_client_connected/5, on_client_subscribe/4, on_client_unsubscribe/4,
         on_client_disconnected/4, on_session_subscribed/4, on_session_unsubscribed/4]).

-define(EMPTY_USERNAME, "").

-define(HOOK_ADD(A, B),      emqx_hooks:add(A, B)).
-define(HOOK_DEL(A, B),      emqx_hooks:del(A, B)).

register_on_message_publish(ScriptName, LuaState) ->
    ?HOOK_ADD('message.publish', {?MODULE, on_message_publish, [ScriptName, LuaState]}).

register_on_message_delivered(ScriptName, LuaState) ->
    ?HOOK_ADD('message.delivered', {?MODULE, on_message_delivered, [ScriptName, LuaState]}).

register_on_message_acked(ScriptName, LuaState) ->
    ?HOOK_ADD('message.acked', {?MODULE, on_message_acked, [ScriptName, LuaState]}).

register_on_client_connected(ScriptName, LuaState) ->
    ?HOOK_ADD('client.connected', {?MODULE, on_client_connected, [ScriptName, LuaState]}).

register_on_client_subscribe(ScriptName, LuaState) ->
    ?HOOK_ADD('client.subscribe', {?MODULE, on_client_subscribe, [ScriptName, LuaState]}).

register_on_client_unsubscribe(ScriptName, LuaState) ->
    ?HOOK_ADD('client.unsubscribe', {?MODULE, on_client_unsubscribe, [ScriptName, LuaState]}).

register_on_client_disconnected(ScriptName, LuaState) ->
    ?HOOK_ADD('client.disconnected', {?MODULE, on_client_disconnected, [ScriptName, LuaState]}).

register_on_session_subscribed(ScriptName, LuaState) ->
    ?HOOK_ADD('session.subscribed', {?MODULE, on_session_subscribed, [ScriptName, LuaState]}).

register_on_session_unsubscribed(ScriptName, LuaState) ->
    ?HOOK_ADD('session.unsubscribed', {?MODULE, on_session_unsubscribed, [ScriptName, LuaState]}).

unregister_hooks({ScriptName, LuaState}) ->
    ?HOOK_DEL('message.publish',      {?MODULE, on_message_publish,      [ScriptName, LuaState]}),
    ?HOOK_DEL('message.delivered',    {?MODULE, on_message_delivered,    [ScriptName, LuaState]}),
    ?HOOK_DEL('message.acked',        {?MODULE, on_message_acked,        [ScriptName, LuaState]}),
    ?HOOK_DEL('client.connected',     {?MODULE, on_client_connected,     [ScriptName, LuaState]}),
    ?HOOK_DEL('client.subscribe',     {?MODULE, on_client_subscribe,     [ScriptName, LuaState]}),
    ?HOOK_DEL('client.unsubscribe',   {?MODULE, on_client_unsubscribe,   [ScriptName, LuaState]}),
    ?HOOK_DEL('client.disconnected',  {?MODULE, on_client_disconnected,  [ScriptName, LuaState]}),
    ?HOOK_DEL('session.subscribed',   {?MODULE, on_session_subscribed,   [ScriptName, LuaState]}),
    ?HOOK_DEL('session.unsubscribed', {?MODULE, on_session_unsubscribed, [ScriptName, LuaState]}).

on_client_connected(#{client_id := ClientId, username := Username}, ConnAck, _ConnAttrs, _ScriptName, LuaState) ->
    ?LOG(debug, "hook client ClientId=~s Username=~s connected with code ~p~n", [ClientId, Username, ConnAck]),
    case catch luerl:call_function([on_client_connected], [ClientId, Username, ConnAck], LuaState) of
        {_Result, _St} ->
            ok;
        Other ->
            ?LOG(error, "lua function on_client_connected() caught exception, ~p", [Other]),
            ok
    end.

on_client_disconnected(#{client_id := ClientId, username := Username}, Error, _ScriptName, LuaState) ->
    ?LOG(debug, "hook client ClientId=~s Username=~s disconnected with ~p~n", [ClientId, Username, Error]),
    case catch luerl:call_function([on_client_disconnected], [ClientId, Username, Error], LuaState) of
        {_Result, _St} ->
            ok;
        Other ->
            ?LOG(error, "lua function on_client_disconnected() caught exception, ~p", [Other]),
            ok
    end.

on_client_subscribe(#{client_id := ClientId, username := Username}, TopicTable, _ScriptName, LuaState) ->
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
    ?LOG(debug, "hook client(~s/~s) will subscribe: ~p~n", [ClientId, Username, Topic]),
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

on_client_unsubscribe(#{client_id := ClientId, username := Username}, TopicTable, _ScriptName, LuaState) ->
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

on_session_subscribed(#{}, TopicItem = {<<$$, _Rest/binary>>, _Opts}, _ScriptName, _LuaState) ->
    %% ignore topics starting with $
    {ok, TopicItem};
on_session_subscribed(#{client_id := ClientId, username := Username}, 
                      TopicItem = {Topic, _Opts}, _ScriptName, LuaState) ->
    ?LOG(debug, "hook session(~s/~s) has subscribed: ~p~n", [ClientId, Username, Topic]),
    case catch luerl:call_function([on_session_subscribed], [ClientId, Username, Topic], LuaState) of
        {_Result, _St} ->
            {ok, TopicItem};
        Other ->
            ?LOG(error, "Topic=~p, lua function on_session_subscribed() caught exception, ~p", [Topic, Other]),
            {ok, TopicItem}
    end.

on_session_unsubscribed(#{}, TopicItem = {<<$$, _Rest/binary>>, _Opts}, _ScriptName, _LuaState) ->
    %% ignore topics starting with $
    {ok, TopicItem};
on_session_unsubscribed(#{client_id := ClientId, username := Username}, 
                        TopicItem = {Topic, _Opts}, _ScriptName, LuaState) ->
    ?LOG(debug, "hook session(~s/~s) has unsubscribed ~p~n", [ClientId, Username, Topic]),
    case catch luerl:call_function([on_session_unsubscribed], [ClientId, Username, Topic], LuaState) of
        {_Result, _St} ->
            {ok, TopicItem};
        Other ->
            ?LOG(error, "Topic=~p, lua function on_session_unsubscribed() caught exception, ~p", [Topic, Other]),
            {ok, TopicItem}
    end.

on_message_publish(Message = #message{topic = <<$$, _Rest/binary>>}, _ScriptName, _LuaState) ->
    %% ignore topics starting with $
    {ok, Message};
on_message_publish(Message = #message{from = {ClientId, Username},
                                      qos = QoS,
                                      flags = Flags = #{retain := Retain},
                                      topic = Topic,
                                      payload = Payload},
                   _ScriptName, LuaState) ->
    ?LOG(debug, "hook message publish ~s~n", [emqx_message:format(Message)]),
    case catch luerl:call_function([on_message_publish], [ClientId, Username, Topic, Payload, QoS, Retain], LuaState) of
        {[false], _St} ->
            {stop, Message};
        {[NewTopic, NewPayload, NewQos, NewRetain], _St} ->
            ?LOG(debug, "lua function on_message_publish() return ~p", [{NewTopic, NewPayload, NewQos, NewRetain}]),
            {ok, Message#message{topic = NewTopic, payload = NewPayload,
                                 qos = round(NewQos), flags = Flags#{retain => to_retain(NewRetain)}}};
        Other ->
            ?LOG(error, "Topic=~p, lua function on_message_publish caught exception, ~p", [Topic, Other]),
            {ok, Message}
    end;
on_message_publish(Message = #message{from = Internal}, _ScriptName, LuaState) ->
    {Status, NewMsg} = on_message_publish(Message#message{from={Internal, ?EMPTY_USERNAME}}, _ScriptName, LuaState),
    {Status, NewMsg#message{from=Internal}}.

on_message_delivered(#{}, #message{topic = <<$$, _Rest/binary>>}, _ScriptName, _LuaState) ->
    %% ignore topics starting with $
    ok;
on_message_delivered(#{client_id := ClientId, username := Username},
                     Message=#message{topic = Topic, payload = Payload, qos = QoS, flags = #{retain := Retain}}, 
                     _ScriptName, LuaState) ->
    ?LOG(debug, "hook message delivered ~s~n", [emqx_message:format(Message)]),
    case catch luerl:call_function([on_message_delivered], [ClientId, Username, Topic, Payload, QoS, Retain], LuaState) of
        {_Result,_St} ->
            ok;
        Other ->
            ?LOG(error, "Topic=~p, lua function on_message_delivered() caught exception, ~p", [Topic, Other]),
            ok
    end.

on_message_acked(#{}, #message{topic = <<$$, _Rest/binary>>}, _ScriptName, _LuaState) ->
    %% ignore topics starting with $
    ok;
on_message_acked(#{client_id := ClientId, username := Username},
                Message=#message{topic = Topic, payload = Payload, qos = QoS, flags = #{retain := Retain}}, _ScriptName, LuaState) ->
    ?LOG(debug, "hook message acked ~s~n", [emqx_message:format(Message)]),
    case catch luerl:call_function([on_message_acked], [ClientId, Username, Topic, Payload, QoS, Retain], LuaState) of
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
    case round(Num) of 0 -> false; _ -> true end.
