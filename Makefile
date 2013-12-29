.PHONY: all
all: test.dot hello
	./hello

dist/setup-config: RISK.hs RISK/*.hs
	cabal install

test.dot hello.s: Test.hs dist/setup-config
	runhaskell -W Test.hs
	dot -Tpng -otest.png test.dot

hello: hello.s
	gcc -o hello hello.s

.PHONY: clean
clean:
	cabal clean
	-rm test.dot
	-rm test.png
	-rm hello
	-rm hello.s

