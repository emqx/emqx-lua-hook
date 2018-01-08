PROJECT = emqx_lua_hook
PROJECT_DESCRIPTION = EMQ X Lua Hook
PROJECT_VERSION = 2.4.1

DEPS = lager luerl
dep_lager = git https://github.com/basho/lager
dep_luerl = git https://github.com/grutabow/luerl

BUILD_DEPS = emqx cuttlefish
dep_emqx = git git@github.com:emqx/emqx-enterprise
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = luerl

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_lua_hook.conf -i priv/emqx_lua_hook.schema -d data
