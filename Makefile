PROGNAME=turnip

GHC_OPTS := \
	-threaded \
	-O3 \
	-Odph \
	-rtsopts \
	-fno-liberate-case \
	-funfolding-use-threshold1000 \
	-funfolding-keeness-factor1000 \
	-fllvm \
	-optlo-O3 \
	-fsimpl-tick-factor=200

GHC_WARNINGS	:= \
	-Werror \
	-fwarn-deprecations \
	-fwarn-duplicate-exports \
	-fwarn-hi-shadowing \
	-fwarn-missing-fields \
	-fwarn-overlapping-patterns \
	-fwarn-type-defaults \
	-fwarn-unused-binds \
	-fwarn-unused-imports \
	-fno-warn-missing-methods

accelerate: accelerate/Main.hs
	ghc $(GHC_OPTS) $(GHC_WARNINGS) --make accelerate/Main.hs

repa: repa/Main.hs
	ghc $(GHC_OPTS) $(GHC_WARNINGS) --make repa/Main.hs

clean:
	rm -f accelerate/*.{hi,o} repa/*.{hi,o} accelerate/Main repa/Main
.PHONY: repa accelerate clean
