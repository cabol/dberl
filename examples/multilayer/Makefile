PROJECT = multilayer

DEPS = jiffy lager dberl cowboy erlydtl

dep_jiffy   = git https://github.com/davisp/jiffy.git       0.13.3
dep_lager   = git https://github.com/basho/lager.git        2.1.1
dep_dberl   = git https://github.com/cabol/dberl.git        master
dep_cowboy  = git https://github.com/extend/cowboy.git      master
dep_erlydtl = git https://github.com/evanmiller/erlydtl.git 0.10.0

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info +fail_on_warning
