PROJECT = emqx_lua_hook
PROJECT_DESCRIPTION = EMQ X Lua Hooks
PROJECT_VERSION = 3.0

DEPS = luerl
dep_luerl = git https://github.com/grutabow/luerl

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqx/emqx emqx30
dep_cuttlefish = git https://github.com/emqx/cuttlefish emqx30

ERLC_OPTS += +debug_info

NO_AUTOPATCH = luerl

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_lua_hook.conf -i priv/emqx_lua_hook.schema -d data
