.PHONY: all
all: risk_sim risk_sim.s example.png
	risk_sim 20

#.PHONY: all
#all: test.dot hello risk_sim.s
#	./hello
#
#hello: hello.s
#	gcc -o hello hello.s

dist/setup-config: RISK.hs RISK/*.hs
	cabal install

example.dot risk_sim.c: Example.hs dist/setup-config
	runhaskell -W Example.hs

example.png: example.dot
	dot -Tpng -oexample.png example.dot

risk_sim: risk_sim.c partitions.c
	gcc -Wall -o risk_sim *.c

risk_sim.s: risk_sim
	otool -tv risk_sim > risk_sim.s

.PHONY: clean
clean:
	cabal clean
	-rm example.dot
	-rm example.png
	-rm risk_api_*
	-rm risk_sim
	-rm risk_sim.c
	-rm risk_sim.s

