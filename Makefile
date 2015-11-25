PROJECT=apns

CONFIG?=priv/app.config

DEPS = jiffy sync
dep_jiffy = git https://github.com/davisp/jiffy 0.14.3
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b

TEST_DEPS = mock_apns
dep_mock_apns = git https://github.com/tomekowal/mockapn.git 4b4c1fd21706060eba2142cc405ce7c8b3396513

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
