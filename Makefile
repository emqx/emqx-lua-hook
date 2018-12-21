PROJECT = emqx_lua_hook
PROJECT_DESCRIPTION = EMQ X Lua Hooks
PROJECT_VERSION = 3.0

DEPS = luerl
dep_luerl = git-emqx https://github.com/grutabow/luerl develop

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx emqx30
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.0

ERLC_OPTS += +debug_info

NO_AUTOPATCH = luerl

define dep_fetch_git-emqx
	git clone -q --depth 1 -b $(call dep_commit,$(1)) -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)) > /dev/null 2>&1; \
	cd $(DEPS_DIR)/$(call dep_name,$(1));
endef

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_lua_hook.conf -i priv/emqx_lua_hook.schema -d data
