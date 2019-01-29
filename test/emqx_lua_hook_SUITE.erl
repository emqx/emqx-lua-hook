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

-module(emqx_lua_hook_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [case01, case02, case03, case04,
     case11, case12, case13,
     case21, case22, case23,
     case31, case32,
     case41, case42, case43,
     case51, case52, case53,
     case61, case62,
     case71, case72, case73,
     case81, case82, case83,
     case101,
     case110, case111, case112, case113, case114, case115,
     case201, case202, case203, case204, case205].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

case01(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_publish(ClientId, Username, topic, payload, qos, retain)"
            "\n    return topic, \"hello\", qos, retain"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{from = {<<"myclient">>, <<"tester">>}, qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg#message{payload = <<"hello">>}}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case02(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_publish(ClientId, Username, topic, payload, qos, retain)"
            "\n    return false"     % return false to stop hook
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{from = {<<"myclient">>, <<"tester">>}, qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({stop, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case03(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =  "function on_message_publish(ClientId, Username, topic, payload, qos, retain)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{from = {<<"myclient">>, <<"tester">>}, qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case04(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_publish(ClientId, Username, topic, payload, qos, retain)"
            "\n    if ClientId == \"broker\" then"
            "\n        return topic, \"hello broker\", qos, retain"
            "\n    else"
            "\n        return false"     % return false to stop hook
            "\n    end"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    %% from is special, not the {ClientId, Username} pattern
    Msg = #message{from = broker, qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg#message{payload = <<"hello broker">>}}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case11(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_delivered(ClientId, Username, topic, payload, qos, retain)"
            "\n    return 0"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_delivered\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.delivered', [#{client_id => <<"myclient">>}], Msg),
    ?assertEqual({ok, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case12(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_delivered(ClientId, Username, topic, payload, qos, retain)"
            "\n    return false"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_delivered\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.delivered', [#{client_id => <<"myclient">>}], Msg),
    ?assertEqual({ok, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case13(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_delivered(ClientId, Username, topic, payload, qos, retain)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_delivered\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.delivered', [#{client_id => <<"myclient">>}], Msg),
    ?assertEqual({ok, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case21(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_acked(ClientId, Username, Topic, Payload, Qos, Retain)"
            "\n    return 0"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_acked\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.acked', [#{client_id => <<"myclient">>}], Msg),
    ?assertEqual({ok, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case22(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_acked(topic, payload, qos, retain)"
            "\n    return false"     % return false to stop hook
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_acked\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.acked', [#{client_id => <<"myclient">>}], Msg),
    ?assertEqual({ok, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case23(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_acked(topic, payload, qos, retain)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_acked\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.acked', [#{client_id => <<"myclient">>}], Msg),
    ?assertEqual({ok, Msg}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case31(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_connected(ClientId, UserName, ReturnCode)"
            "\n    return 0"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_connected\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    ?assertEqual(ok, 
                 emqx_hooks:run('client.connected', 
                                [#{client_id => <<"myclient">>, username => <<"tester">>}, 0, []])),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case32(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_connected(topic, payload, qos, retain)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_connected\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    ?assertEqual(ok, 
                 emqx_hooks:run('client.connected', 
                                [#{client_id => <<"myclient">>, username => <<"tester">>}, 0, []])),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case41(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_subscribe(ClientId, Username, Topic)"
            "\n    if Topic == \"a/b/c\" then"
            "\n        Topic = \"a1/b1/c1\";"
            "\n    end"
            "\n    return Topic"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_subscribe\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    TopicTable = [{<<"a/b/c">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}],
    Ret = emqx_hooks:run('client.subscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({ok, [{<<"a1/b1/c1">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}]}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case42(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_subscribe(ClientId, Username, Topic)"
            "\n    return false"     % return false to stop hook
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_subscribe\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    TopicTable = [{<<"a/b/c">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}],
    Ret = emqx_hooks:run('client.subscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({stop, TopicTable}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case43(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_subscribe(ClientId, Username, Topic)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_subscribe\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    TopicTable = [{<<"a/b/c">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}],
    Ret = emqx_hooks:run('client.subscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({ok, TopicTable}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case51(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_unsubscribe(ClientId, Username, Topic)"
            "\n    if Topic == \"a/b/c\" then"
            "\n        Topic = \"a1/b1/c1\";"
            "\n    end"
            "\n    return Topic"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_unsubscribe\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    TopicTable = [{<<"a/b/c">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}],
    Ret = emqx_hooks:run('client.unsubscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({ok, [{<<"a1/b1/c1">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}]}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case52(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_unsubscribe(ClientId, Username, Topic)"
            "\n    return false"     % return false to stop hook
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_unsubscribe\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    TopicTable = [{<<"a/b/c">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}],
    Ret = emqx_hooks:run('client.unsubscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({stop, TopicTable}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case53(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_unsubscribe(ClientId, Username, Topic)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_unsubscribe\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    TopicTable = [{<<"a/b/c">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}],
    Ret = emqx_hooks:run('client.unsubscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({ok, TopicTable}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case61(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_disconnected(ClientId, UserName, Error)"
            "\n    return 0"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_disconnected\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    ?assertEqual(ok, 
                 emqx_hooks:run('client.disconnected', 
                                [#{client_id => <<"myclient">>, username => <<"tester">>}, 0])),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case62(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_client_disconnected(ClientId, UserName, Error)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_disconnected\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    ?assertEqual(ok, 
                 emqx_hooks:run('client.disconnected', 
                                [#{client_id => <<"myclient">>, username => <<"tester">>}, 0])),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case71(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_session_subscribed(ClientId, Username, Topic)"
            "\n    return 0"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_session_subscribed\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.subscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case72(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_session_subscribed(ClientId, Username, Topic)"
            "\n    return false"     % return false to stop hook
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_session_subscribed\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.subscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case73(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_session_subscribed(ClientId, Username, Topic)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_session_subscribed\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.subscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case81(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_session_unsubscribed(ClientId, Username, Topic)"
            "\n    return 0"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_session_unsubscribed\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.unsubscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case82(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_session_unsubscribed(ClientId, Username, Topic)"
            "\n    return false"     % return false to stop hook
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_session_unsubscribed\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.unsubscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case83(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_session_unsubscribed(ClientId, Username, Topic)"
            "\n    return 9/0"     % this code has fatal error
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_session_unsubscribed\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.unsubscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case101(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    ScriptName2 = "hook_lua/mn.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
            "\n    return topic, \"hello\", qos, retain"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),

    Code2 =    "function on_client_subscribe(ClientId, Username, Topic)"
            "\n    if Topic == \"a/b/c\" then"
            "\n        Topic = \"a1/b1/c1\";"
            "\n    end"
            "\n    return Topic"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_client_subscribe\""
            "\nend",
    ok = file:write_file(ScriptName2, Code2),

    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),

    Ret = emqx_hooks:run('message.publish',[], #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>}),
    ?assertEqual({ok, #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"hello">>}}, Ret),

    TopicTable = [{<<"a/b/c">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}],
    Ret2 = emqx_hooks:run('client.subscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({ok, [{<<"a1/b1/c1">>, [qos, 1]}, {<<"d/+/e">>, [{qos, 2}]}]}, Ret2),

    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName),
    ok = file:delete(ScriptName2).

case110(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua",
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
            "\n    return \"changed/topic\", \"hello\", qos, retain"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg#message{topic = <<"changed/topic">>, payload = <<"hello">>}}, Ret),
    application:stop(emqx_lua_hook),
    timer:sleep(700).

case111(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua",
    Code =    "function on_message_publish(topic, payload, qos, retain)"
            "\n    return \"changed/topic\", \"hello\", qos, retain"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),
    emqx_ctl:run_command(["luahook", "unload", "abc"]),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg}, Ret),
    application:stop(emqx_lua_hook),
    timer:sleep(700).

case112(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua",
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
            "\n    return \"changed/topic\", \"hello\", qos, retain"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),
    emqx_ctl:run_command(["luahook", "unload", "abc"]),
    timer:sleep(100),
    emqx_ctl:run_command(["luahook", "load", "abc"]),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg#message{topic = <<"changed/topic">>, payload = <<"hello">>}}, Ret),
    application:stop(emqx_lua_hook),
    timer:sleep(700).

case113(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua",
    ScriptDisabled = ScriptName ++ ".x",
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
                "\n    return \"changed/topic\", \"hello\", qos, retain"
                "\nend"
                "\n"
                "\nfunction register_hook()"
                "\n    return \"on_message_publish\""
                "\nend",
    ok = file:write_file(ScriptName, Code),
    file:delete(ScriptDisabled),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),
    emqx_ctl:run_command(["luahook", "disable", "abc"]),   % this command will rename "abc.lua" to "abc.lua.x"
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg}, Ret),
    application:stop(emqx_lua_hook),
    true = filelib:is_file(ScriptDisabled),
    file:delete(ScriptDisabled),
    timer:sleep(700).

case114(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua.x",   % disabled script
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
            "\n    return \"changed/topic\", \"hello\", qos, retain"
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\""
            "\nend",
    ok = file:write_file(ScriptName, Code),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),
    emqx_ctl:run_command(["luahook", "enable", "abc"]),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg#message{topic = <<"changed/topic">>, payload = <<"hello">>}}, Ret),
    application:stop(emqx_lua_hook),
    timer:sleep(700).

case115(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua",
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
            "\n    return \"changed/topic\", \"hello\", qos, retain"
            "\nend"
            "\n"
            "function on_client_subscribe(ClientId, Username, Topic)"
            "\n    return \"play/football\""
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\", \"on_client_subscribe\""
            "\nend",
    ok = file:write_file(ScriptName, Code),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),
    emqx_ctl:run_command(["luahook", "reload", "abc"]),
    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg#message{topic = <<"changed/topic">>, payload = <<"hello">>}}, Ret),

    TopicTable = [{<<"d/+/e">>, [{qos, 2}]}],
    Ret2 = emqx_hooks:run('client.subscribe',[#{client_id => <<"myclient">>}], TopicTable),
    ?assertEqual({ok, [{<<"play/football">>, [{qos, 2}]}]}, Ret2),

    application:stop(emqx_lua_hook),
    timer:sleep(700).

case201(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function on_session_subscribed(ClientId, Username, Topic)"
            "\n    return 0"
            "\nend"
            "\n"
            "\nfunction on_session_subscribed1()"  % register_hook() is missing
            "\n    return \"on_session_subscribed\""
            "\nend",
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.subscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case202(_Config) ->
    ScriptName = "hook_lua/abc.lua",
    emqx_hooks:start_link(),
    ok = filelib:ensure_dir("hook_lua/a"),
    Code =    "function abc(ClientId, Username, Topic)"
            "\n    return 0"
            "\nend"
            "\n"
            "\n9/0",   % error code
    ok = file:write_file(ScriptName, Code),
    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.subscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop(),
    ok = file:delete(ScriptName).

case203(_Config) ->
    file:del_dir("hook_lua"),  % if this dir is not exist, what will happen?
    emqx_hooks:start_link(),

    emqx_lua_hook:start_link(),
    emqx_lua_hook:load_scripts(),
    Topic = {<<"a/b/c">>, [qos, 1]},
    Ret = emqx_hooks:run('session.subscribed',[#{client_id => <<"myclient">>}], Topic),
    ?assertEqual({ok, Topic}, Ret),
    emqx_lua_hook:stop().

case204(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua",
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
            "\n    return topic, payload .. \"_Z\", qos, retain"
            "\nend"
            "\n"
            "function on_client_subscribe(ClientId, Username, Topic)"
            "\n    return \"play/football\""
            "\nend"
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\", \"on_client_subscribe\", \"on_message_publish\""  % if 2 on_message_publish() are registered, what will happend?
            "\nend",
    ok = file:write_file(ScriptName, Code),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),

    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg#message{payload = <<"123_Z">>}}, Ret),

    application:stop(emqx_lua_hook),
    timer:sleep(700).

case205(_Config) ->
    ok = filelib:ensure_dir("hook_lua/a"),
    ScriptName = "hook_lua/abc.lua",
    Code =    "function on_message_publish(clientid, username, topic, payload, qos, retain)"
            "\n    return topic, \"hello\", qos, retain"
            "\nend_with_error"  %% syntax error
            "\n"
            "\nfunction register_hook()"
            "\n    return \"on_message_publish\", \"on_client_subscribe\", \"on_message_publish\""  % if 2 on_message_publish() are registered, what will happend?
            "\nend",
    ok = file:write_file(ScriptName, Code),

    emqx_hooks:start_link(),
    emqx_ctl:start_link(),
    {ok,_} = application:ensure_all_started(emqx_lua_hook),

    Msg = #message{qos = 2, flags = #{retain => true}, topic = <<"a/b/c">>, payload = <<"123">>},
    Ret = emqx_hooks:run('message.publish',[], Msg),
    ?assertEqual({ok, Msg}, Ret),

    application:stop(emqx_lua_hook),
    timer:sleep(700).
