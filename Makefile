PULSAR=./pulsar
BEAM=./beam

TESTS=$(subst .pul,.res,$(wildcard examples/*.pul) $(wildcard tests/*.pul))

.PHONY: all clean watch test

all:
	jbuilder build @install warp/warp.cma # warp.cma for toplevel use
	[ -h $(PULSAR) ] || ln -s _build/install/default/bin/pulsar $(PULSAR)
	[ -h $(BEAM) ] || ln -s _build/install/default/bin/pulsar-beam $(BEAM)

clean:
	rm -rf _build
	rm -f pulsar.install
	[ -h $(PULSAR) ] && unlink $(PULSAR) || :
	[ -h $(BEAM) ] && unlink $(BEAM) || :
	find -name ".merlin" -exec rm {} \;

watch:
	while find \( -iname "*.ml" -or -iname "*.mli" \
		  -or -iname "*.mly" \) -print0  \
		| xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make all; \
	done

test: all $(TESTS)

%.res: %.pul
	$(PULSAR) $^ > $@
