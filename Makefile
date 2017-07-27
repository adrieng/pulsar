.PHONY: all clean watch test

all:
	jbuilder build @install warp/warp.cma # warp.cma for toplevel use
	[ -h pulsar ] || ln -s _build/install/default/bin/pulsar pulsar
	[ -h beam ] || ln -s _build/install/default/bin/pulsar-beam beam

clean:
	rm -rf _build
	rm -f pulsar.install
	[ -h pulsar ] && unlink pulsar
	[ -h beam ] && unlink beam
	find -name ".merlin" -exec rm {} \;

watch:
	while find \( -iname "*.ml" -or -iname "*.mli" \
		  -or -iname "*.mly" \) -print0  \
		| xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make all; \
	done

test: all
	./pulsar examples/*.pul
