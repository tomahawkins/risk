.PHONY: all
all:
	cabal install
	runhaskell -W Test.hs > test.dot
	dot -Tpng -otest.png test.dot

.PHONY: clean
clean:
	cabal clean
	-rm test.dot
	-rm test.png

