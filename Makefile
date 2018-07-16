PROJECT = emqx_lua_hook
PROJECT_DESCRIPTION = EMQ X Lua Hooks
PROJECT_VERSION = 3.0

DEPS = lager luerl
dep_lager = git https://github.com/erlang-lager/lager 3.6.4
dep_luerl = git https://github.com/grutabow/luerl

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqx/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = luerl

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_lua_hook.conf -i priv/emqx_lua_hook.schema -d data
