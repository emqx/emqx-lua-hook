PROJECT = emq_lua_hook
PROJECT_DESCRIPTION = EMQ Hooks in lua
PROJECT_VERSION = 2.3.9

DEPS = lager luerl
dep_lager = git https://github.com/basho/lager
dep_luerl = git https://github.com/grutabow/luerl

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd emq24
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = luerl

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_lua_hook.conf -i priv/emq_lua_hook.schema -d data
