%%--------------------------------------------------------------------
%% Copyright (c) 2016-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emq_lua_hook_cli).

-author("Feng Lee <feng@emqtt.io>").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([stop/0, loadall/0, unloadall/0, load/1, unload/1]).

-export([load_cmd/0, cmd/1, unload_cmd/0]).

-include("emq_lua_hook.hrl").
-include_lib("luerl/src/luerl.hrl").

-define(LUA_DIR, "hook_lua/").
-define(LUA_WILD, ?LUA_DIR++"*.lua").
-define(PRINT(Format, Args), io:format(Format, Args)).
-define(PRINT_CMD(Cmd, Descr), io:format("~-48s# ~s~n", [Cmd, Descr])).
-define(USAGE(CmdList), [?PRINT_CMD(Cmd, Descr) || {Cmd, Descr} <- CmdList]).

-record(state, {loaded_scripts = []}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

stop() ->
    gen_server:call(?MODULE, stop).

loadall() ->
    gen_server:call(?MODULE, loadall).

unloadall() ->
    gen_server:call(?MODULE, unloadall).

load(ScriptName) ->
    gen_server:call(?MODULE, {load, ScriptName}).

unload(ScriptName) ->
    gen_server:call(?MODULE, {unload, ScriptName}).

load_cmd() ->
    emqttd_ctl:register_cmd(luahook, {?MODULE, cmd}, []).

unload_cmd() ->
    emqttd_ctl:unregister_cmd(luahook).

cmd(["load", Script]) ->
    case load(fullname(Script)) of
        ok -> ?PRINT("success to load ~p~n", [Script]);
        error -> ?PRINT("fail to load ~p~n", [Script])
    end;

cmd(["reload", Script]) ->
    FullName = fullname(Script),
    unload(FullName),
    case load(FullName) of
        ok -> ?PRINT("success to reload ~p~n", [Script]);
        error -> ?PRINT("fail to reload ~p~n", [Script])
    end;

cmd(["unload", Script]) ->
    unload(fullname(Script)),
    ?PRINT("success to unload ~p~n", [Script]);

cmd(["enable", Script]) ->
    FullName = fullname(Script),
    case file:rename(fullnamedisable(Script), FullName) of
        ok ->
            case load(FullName) of
                ok -> ?PRINT("success to enable ~p~n", [Script]);
                error -> ?PRINT("fail to enable ~p~n", [Script])
            end;
        {error, Reason} -> ?PRINT("fail to enable ~p due to ~p~n", [Script, Reason])
    end;

cmd(["disable", Script]) ->
    FullName = fullname(Script),
    unload(FullName),
    case file:rename(FullName, fullnamedisable(Script)) of
        ok -> ?PRINT("success to disable ~p~n", [Script]);
        {error, Reason} -> ?PRINT("fail to disable ~p due to ~p~n", [Script, Reason])
    end;

cmd(_) ->
    ?USAGE([{"luahook load <Script>",     "load lua script into hook"},
        {"luahook unload <Script>",       "unload lua script from hook"},
        {"luahook reload <Script>",       "reload lua script into hook"},
        {"luahook enable <Script>",       "enable lua script and load it into hook"},
        {"luahook disable <Script>",      "unload lua script out of hook and disable it"}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({}) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(loadall, _From, State) ->
    {reply, ok, State#state{loaded_scripts = do_loadall()}, hibernate};

handle_call(unloadall, _From, State=#state{loaded_scripts = Scripts}) ->
    do_unloadall(Scripts),
    {reply, ok, State#state{loaded_scripts = []}, hibernate};

handle_call({load, ScriptName}, _From, State=#state{loaded_scripts = Scripts}) ->
    {Ret, NewScripts} = case do_load(ScriptName) of
                            error -> {error, Scripts};
                            ScriptName ->
                                case lists:member(ScriptName, Scripts) of
                                    true -> {ok, Scripts};
                                    false -> {ok, lists:append([ScriptName], Scripts)}
                                end
                        end,
    {reply, Ret, State#state{loaded_scripts = NewScripts}, hibernate};

handle_call({unload, ScriptName}, _From, State=#state{loaded_scripts = Scripts}) ->
    do_unload(ScriptName),   % Unload first! If this gen_server has been crashed, loaded_scripts will be empty
    NewScripts =    case lists:member(ScriptName, Scripts) of
                        true -> lists:delete(ScriptName, Scripts);
                        false -> Scripts
                    end,
    {reply, ok, State#state{loaded_scripts = NewScripts}, hibernate};

handle_call(Request, From, State) ->
    ?LOG(error, "Unknown Request=~p from ~p", [Request, From]),
    {reply, ok, State, hibernate}.

handle_cast(Msg, State) ->
    ?LOG(error, "Bridge Unknown cast Msg=~p", [Msg]),
    {noreply, State, hibernate}.

handle_info(Info, State) ->
    ?LOG(error, "Bridge, Unknown Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{loaded_scripts = Scripts}) ->
    do_unloadall(Scripts),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_loadall() ->
    FileList = filelib:wildcard(?LUA_WILD),
    List = [do_load(X)||X<-FileList],
    [X||X<-List, is_list(X)].

do_load(FileName) ->
    case catch luerl:dofile(FileName) of
        {'EXIT', St00} ->
            ?LOG(error, "Failed to load lua script ~p due to error ~p", [FileName, St00]),
            error;
        {_Ret, St0=#luerl{}} ->
            case catch luerl:call_function([register_hook], [], St0) of
                {'EXIT', St1} ->
                    ?LOG(error, "Failed to execute register_hook function in lua script ~p, which has syntax error, St1=~p", [FileName, St1]),
                    error;
                {Ret1, St1} ->
                    ?LOG(debug, "Register lua script ~p", [FileName]),
                    do_register_hooks(Ret1, St1, FileName),
                    FileName;
                Other ->
                    ?LOG(error, "Failed to load lua script ~p, register_hook() raise exception ~p", [FileName, Other]),
                    error
            end;
        Exception ->
            ?LOG(error, "Failed to load lua script ~p with error ~p", [FileName, Exception]),
            error
    end.


do_register(<<"on_message_publish">>, St, ScriptName) ->
    emq_lua_script:register_on_message_publish(ScriptName, St);
do_register(<<"on_message_delivered">>, St, ScriptName) ->
    emq_lua_script:register_on_message_delivered(ScriptName, St);
do_register(<<"on_message_acked">>, St, ScriptName) ->
    emq_lua_script:register_on_message_acked(ScriptName, St);
do_register(<<"on_client_connected">>, St, ScriptName) ->
    emq_lua_script:register_on_client_connected(ScriptName, St);
do_register(<<"on_client_subscribe">>, St, ScriptName) ->
    emq_lua_script:register_on_client_subscribe(ScriptName, St);
do_register(<<"on_client_unsubscribe">>, St, ScriptName) ->
    emq_lua_script:register_on_client_unsubscribe(ScriptName, St);
do_register(<<"on_client_disconnected">>, St, ScriptName) ->
    emq_lua_script:register_on_client_disconnected(ScriptName, St);
do_register(<<"on_session_subscribed">>, St, ScriptName) ->
    emq_lua_script:register_on_session_subscribed(ScriptName, St);
do_register(<<"on_session_unsubscribed">>, St, ScriptName) ->
    emq_lua_script:register_on_session_unsubscribed(ScriptName, St);
do_register(Hook, _St, ScriptName) ->
    ?LOG(error, "Discard unknown hook ~p ScriptName=~p", [Hook, ScriptName]).

do_register_hooks([], _St, _ScriptName) ->
    ok;
do_register_hooks([H|T], St, ScriptName) ->
    do_register(H, St, ScriptName),
    do_register_hooks(T, St, ScriptName);
do_register_hooks(Hook = <<$o, $n, _Rest/binary>>, St, ScriptName) ->
    do_register(Hook, St, ScriptName);
do_register_hooks(Hook, _St, ScriptName) ->
    ?LOG(error, "Discard unknown hook type ~p from ~p", [Hook, ScriptName]).

do_unloadall(Scripts) ->
    [do_unload(X) || X <- Scripts],
    ok.

do_unload(ScriptName) ->
    emq_lua_script:unregister_hooks(ScriptName).

fullname(Script) ->
    ?LUA_DIR++Script++".lua".
fullnamedisable(Script) ->
    fullname(Script)++".x".
