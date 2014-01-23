.PHONY: all
all: test.dot hello risk_sim.s
	./hello

.PHONY: run
run: risk_sim risk_sim.s
	risk_sim

dist/setup-config: RISK.hs RISK/*.hs
	cabal install

test.dot hello.s risk_sim.c: Test.hs dist/setup-config
	runhaskell -W Test.hs
	dot -Tpng -otest.png test.dot

hello: hello.s
	gcc -o hello hello.s

risk_sim: risk_sim.c partitions.c
	gcc -Wall -o risk_sim *.c

risk_sim.s: risk_sim
	otool -tv risk_sim > risk_sim.s

.PHONY: clean
clean:
	cabal clean
	-rm test.dot
	-rm test.png
	-rm hello
	-rm hello.s
	-rm risk_api_*
	-rm risk_sim
	-rm risk_sim.c
	-rm risk_sim.s

