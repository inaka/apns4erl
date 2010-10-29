ERL	:=  erl
ERLC	:=  erlc -W -I include -v -o ebin
SOURCES	:=  src/*.erl
EPATH	:=  -pa ebin
DOC_OPTS:=  {dir, \"doc/html\"}, {includes, [\"include\"]}, {source_path, [\"include\", \"src\"]}

all:
	@mkdir -p ebin
	@$(ERL) $(EPATH) -noinput +B \
		-eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs: all
	@rm -rf web/*
	@mkdir -p doc/html
	@cp src/overview.edoc doc/html
	@$(ERL) -noshell $(EPATH) \
		-eval "edoc:files(filelib:wildcard(\"$(SOURCES)\") , [$(DOC_OPTS)])" \
		-s init stop

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam
	@rm -fv erl_crash.dump ebin/erl_crash.dump

run: all
	$(ERL) -sname "$(PROJECT)" $(EPATH)

test: all
	@$(ERL) -noshell $(EPATH) -s $(PROJECT)_test test -s init stop

distclean:
	@echo "removing:"
	@rm -fv ebin/*.beam
	@rm -fv doc/*
	@rm -fv erl_crash.dump ebin/erl_crash.dump

docclean:
	@echo "removing:"
	@rm -fv doc/html/*
