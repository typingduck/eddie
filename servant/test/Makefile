EMULATOR=jam

MODULES=script

FILES=$(MODULES:%=%.$(EMULATOR)) start.boot

ERLC_FLAGS=-W

all: $(FILES)

clean:
	rm -f $(FILES) start.boot start.script

# Erlang

%.jam: %.erl
	erlc -b jam $(ERLC_FLAGS) $<

%.beam: %.erl
	erlc -b beam $(ERLC_FLAGS) $<

# Misc

start.boot: start.rel
	erl -noinput -s script make start local -s init stop
