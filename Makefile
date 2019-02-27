.PHONY: gen.emqx.conf
PROJECT = emqx_lua_hook
PROJECT_DESCRIPTION = EMQ X Lua Hooks

DEPS = luerl
dep_luerl = git-emqx https://github.com/grutabow/luerl develop

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop testing), $(CUR_BRANCH), testing)

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1

ERLC_OPTS += +debug_info

NO_AUTOPATCH = luerl cuttlefish

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)

include erlang.mk

CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

ct: app.config

app.config: $(CUTTLEFISH_SCRIPT) gen.emqx.conf
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/emqx_lua_hook.conf -i priv/emqx_lua_hook.schema -d data

$(CUTTLEFISH_SCRIPT): rebar-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

bbmustache:
	$(verbose) git clone https://github.com/soranoba/bbmustache.git && cd bbmustache && ./rebar3 compile && cd ..

gen.emqx.conf: bbmustache deps/emqx/etc/emqx.conf
	$(verbose) erl -noshell -pa bbmustache/_build/default/lib/bbmustache/ebin -eval \
		"{ok, Temp} = file:read_file('deps/emqx/etc/emqx.conf'), \
		{ok, Vars0} = file:consult('vars'), \
		Vars = [{atom_to_list(N), list_to_binary(V)} || {N, V} <- Vars0], \
		Targ = bbmustache:render(Temp, Vars), \
		ok = file:write_file('deps/emqx/etc/gen.emqx.conf', Targ), \
		halt(0)."

distclean::
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json cuttlefish

rebar-deps:
	rebar3 get-deps

rebar-clean:
	@rebar3 clean

rebar-compile: rebar-deps
	rebar3 compile

rebar-eunit: $(CUTTLEFISH_SCRIPT)
	@rebar3 eunit

rebar-cover:
	@rebar3 cover

coveralls:
	@rebar3 coveralls send

rebar-ct: app.config
	rebar3 ct

rebar-xref:
	@rebar3 xref
