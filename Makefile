.PHONY: all clean

all:
	jbuilder build @install clock/clock.cma # clock.cma for toplevel use

clean:
	rm -rf _build
	rm -f pulsar.install
	find -name ".merlin" -exec rm {} \;
