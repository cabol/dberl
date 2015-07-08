PROJECT = dberl

CONFIG ?= test/test.config

DEPS = jiffy cberl erlydtl
TEST_DEPS =

dep_jiffy   = git https://github.com/davisp/jiffy.git       0.13.3
dep_cberl   = git https://github.com/chitika/cberl.git      master
dep_erlydtl = git https://github.com/evanmiller/erlydtl.git 0.10.0

DIALYZER_Ddberl := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk

ERLC_OPTS += +debug_info +fail_on_warning

TEST_ERLC_OPTS += +debug_info +fail_on_warning
CT_SUITES = dberl_couchbase
CT_OPTS += -cover test/cover.spec -erl_args -s dberl -config ${CONFIG}

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config ${CONFIG}

test-shell: build-ct-suites app
	erl -pa ebin -pa deps/*/ebin -pa test -s dberl -config ${CONFIG}

devtests: tests
	open logs/index.html

quicktests: app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam

erldocs: app
	erldocs . -o doc/
