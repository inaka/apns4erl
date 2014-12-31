PROJECT=apns

CONFIG?=priv/app.config

DEPS = jiffy sync katana eper
dep_jiffy = git https://github.com/davisp/jiffy 0.13.3
dep_sync = git https://github.com/inaka/sync.git 0.1
dep_katana =  git https://github.com/inaka/erlang-katana 0.2.0
dep_eper = git https://github.com/massemanet/eper.git 0.90.0

include erlang.mk

ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

CT_SUITES = apns
CT_OPTS = -cover test/apns.coverspec -vvv -erl_args -config test/app.config

shell: app
	erl -pa ebin -pa deps/*/ebin -s crypto -s inets -s ssl -s sync -s apns -config ${CONFIG}

erldocs: app
	erldocs . -o doc/
