.PHONY: all clean watch

all:
	jbuilder build @install warp/warp.cma # warp.cma for toplevel use

clean:
	rm -rf _build
	rm -f pulsar.install
	find -name ".merlin" -exec rm {} \;

watch:
	while find \( -iname "*.ml" -or -iname "*.mli" \
		  -or -iname "*.mly" \) -print0  \
		| xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make all; \
	done
