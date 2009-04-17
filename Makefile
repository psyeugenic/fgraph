
all: Makefile
	erlc -oebin -Iinclude src/fgraph.erl
	erlc -oebin -Iinclude src/fgraph_panel.erl
	erlc -oebin -Iinclude src/test.erl

clean:
	rm ebin/*.beam
