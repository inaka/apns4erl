PROJECT = apns
PROJECT_VERSION = $(shell git describe --tag --abbrev=0)

app:: rebar.config

LOCAL_DEPS = inets sasl
DEPS = gun jsx base64url lager uuid
dep_lager = git https://github.com/erlang-lager/lager 3.6.2
dep_gun = git https://github.com/ninenines/gun.git 1.3.0
dep_jsx = git https://github.com/talentdeficit/jsx.git 2.9.0
dep_base64url = git https://github.com/dvv/base64url.git v1.0
dep_uuid = git https://github.com/okeuday/uuid.git v1.7.4

include erlang.mk

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
