PROJECT = unravel
DEPS = cowboy
include erlang.mk

.PHONY: frontend

all:: frontend

clean::
	make -C priv/elm/ clean

frontend:
	make -C priv/elm/
