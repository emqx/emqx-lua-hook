PROJECT = emqx_lua_hook
PROJECT_DESCRIPTION = EMQ X Lua Hooks
PROJECT_VERSION = 3.1

DEPS = luerl
dep_luerl = git-emqx https://github.com/grutabow/luerl develop

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx develop
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1

ERLC_OPTS += +debug_info

NO_AUTOPATCH = luerl

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_lua_hook.conf -i priv/emqx_lua_hook.schema -d data
