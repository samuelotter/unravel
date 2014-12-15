PROJECT = unravel
DEPS = cowboy gproc jiffy
include erlang.mk

.PHONY: frontend

all:: frontend

clean::
	make -C priv/elm/ clean

frontend:
	make -C priv/elm/
