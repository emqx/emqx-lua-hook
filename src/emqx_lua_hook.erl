%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_lua_hook).

-behaviour(gen_server).

-include("emqx_lua_hook.hrl").
-include_lib("luerl/src/luerl.hrl").
-define(LUA_DIR, "hook_lua/").
-define(LUA_WILD, ?LUA_DIR++"*.lua").

-export([start_link/0, stop/0]).
-export([load_scripts/0, unload_scrips/0, load_script/1, unload_script/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {loaded_scripts = []}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

stop() ->
    gen_server:call(?SERVER, stop).

load_scripts() ->
    gen_server:call(?SERVER, load_scripts).

unload_scrips() ->
    gen_server:call(?SERVER, unload_scrips).

load_script(ScriptName) ->
    gen_server:call(?SERVER, {load_script, ScriptName}).

unload_script(ScriptName) ->
    gen_server:call(?SERVER, {unload_script, ScriptName}).

%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------

init({}) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(load_scripts, _From, State) ->
    {reply, ok, State#state{loaded_scripts = do_loadall()}, hibernate};

handle_call(unload_scrips, _From, State=#state{loaded_scripts = Scripts}) ->
    do_unloadall(Scripts),
    {reply, ok, State#state{loaded_scripts = []}, hibernate};

handle_call({load, ScriptName}, _From, State=#state{loaded_scripts = Scripts}) ->
    {Ret, NewScripts} = case do_load(ScriptName) of
                            error -> {error, Scripts};
                            ScriptName ->
                                case lists:member(ScriptName, Scripts) of
                                    true  -> {ok, Scripts};
                                    false -> {ok, lists:append([ScriptName], Scripts)}
                                end
                        end,
    {reply, Ret, State#state{loaded_scripts = NewScripts}, hibernate};

handle_call({unload, ScriptName}, _From, State=#state{loaded_scripts = Scripts}) ->
    do_unload(ScriptName), % Unload first! If this gen_server has been crashed, loaded_scripts will be empty
    NewScripts = case lists:member(ScriptName, Scripts) of
                     true -> lists:delete(ScriptName, Scripts);
                     false -> Scripts
                 end,
    {reply, ok, State#state{loaded_scripts = NewScripts}, hibernate};

handle_call(Request, From, State) ->
    ?LOG(error, "Unknown Request=~p from ~p", [Request, From]),
    {reply, ignored, State, hibernate}.

handle_cast(Msg, State) ->
    ?LOG(error, "unexpected cast: ~p", [Msg]),
    {noreply, State, hibernate}.

handle_info(Info, State) ->
    ?LOG(error, "unexpected info: ~p", [Info]),
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
    emqx_lua_script:register_on_message_publish(ScriptName, St);
do_register(<<"on_message_delivered">>, St, ScriptName) ->
    emqx_lua_script:register_on_message_delivered(ScriptName, St);
do_register(<<"on_message_acked">>, St, ScriptName) ->
    emqx_lua_script:register_on_message_acked(ScriptName, St);
do_register(<<"on_client_connected">>, St, ScriptName) ->
    emqx_lua_script:register_on_client_connected(ScriptName, St);
do_register(<<"on_client_subscribe">>, St, ScriptName) ->
    emqx_lua_script:register_on_client_subscribe(ScriptName, St);
do_register(<<"on_client_unsubscribe">>, St, ScriptName) ->
    emqx_lua_script:register_on_client_unsubscribe(ScriptName, St);
do_register(<<"on_client_disconnected">>, St, ScriptName) ->
    emqx_lua_script:register_on_client_disconnected(ScriptName, St);
do_register(<<"on_session_subscribed">>, St, ScriptName) ->
    emqx_lua_script:register_on_session_subscribed(ScriptName, St);
do_register(<<"on_session_unsubscribed">>, St, ScriptName) ->
    emqx_lua_script:register_on_session_unsubscribed(ScriptName, St);
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
    emqx_lua_script:unregister_hooks(ScriptName).