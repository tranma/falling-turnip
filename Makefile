PROGNAME=turnip

GHC_OPTS := \
	-threaded \
	-O3 \
	-Odph \
	-rtsopts \
	-fllvm \

GHC_WARNINGS	:= \
	-Werror \
	-fwarn-deprecations \
	-fwarn-duplicate-exports \
	-fwarn-hi-shadowing \
	-fwarn-missing-fields \
	-fwarn-overlapping-patterns \
	-fwarn-type-defaults \
	-fwarn-unused-imports \
	-fno-warn-missing-methods

$(PROGNAME): accelerate/Main.hs
	ghc $(GHC_OPTS) $(GHC_WARNINGS) --make accelerate/Main.hs

clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
