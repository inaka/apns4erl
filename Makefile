RUN := +Bc +K true -smp enable -pa ebin -s crypto -s inets -s ssl

all:
	./rebar get-deps && ./rebar compile

compile:
	./rebar compile

clean:
	./rebar clean

build_plt: all
	dialyzer --verbose --build_plt --apps kernel stdlib erts compiler hipe crypto \
		edoc gs syntax_tools --output_plt ~/.apns4erl.plt

analyze: all
	dialyzer --verbose --plt ~/.apns4erl.plt -Werror_handling ebin

xref: all
	./rebar skip_deps=true --verbose xref
   
shell: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN};\
	else\
		erl  -boot start_sasl ${RUN};\
	fi

run: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN} -s apns;\
	else\
		erl  -boot start_sasl ${RUN} -s apns;\
	fi

test: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run apns_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run apns_tests main;\
	fi

doc: compile
	./rebar skip_deps=true doc

