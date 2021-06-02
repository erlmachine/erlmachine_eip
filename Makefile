PROJECT = erlmachine_eip
PROJECT_DESCRIPTION = Erlmachine extensions to implement EIP
PROJECT_VERSION = 1.0.0

DEPS = erlmachine

dep_erlmachine = git https://github.com/Erlmachine/erlmachine

include erlang.mk
