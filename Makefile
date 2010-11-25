
all: Makefile
	erlc -oebin -Iinclude src/fgraph.erl
	erlc -oebin -Iinclude src/fgraph_win.erl
	erlc -oebin -Iinclude src/percept_win.erl
	erlc -oebin -Iinclude src/processes.erl

clean:
	rm ebin/*.beam
