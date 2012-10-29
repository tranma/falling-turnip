PROGNAME=psand
GHC_OPTS=-threaded -O2 -Odph -rtsopts -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3

$(PROGNAME): Main.hs
	ghc $(GHC_OPTS) --make Main.hs

clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
