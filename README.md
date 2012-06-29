# fgraph #

Some of these files already are in the reltool application. I originally wrote it just for fun but it ended up to show dependecies in reltool.


# Processes v1.0 #

Processes is a process visualization tool. It can visualize the processeses and its ancestry in a system or the links between the processes. It will also show messages between processes.

## Getting Started v1.0 ##

Clone, Checkout and Make:

    git clone git://github.com/psyeugenic/fgraph.git
    git checkout 1.0
    make

Start a new erlang node:

    erl -pa ebin -sname view

In the erlang shell type:

    processes:start().


A wxErlang window will appear and you can now connect to a remote node by clicking File in the meny and then Connect. Type in the remote node name, e.g.: mynode@hostname

The result can look like this:
![processes-mnesia](https://github.com/psyeugenic/fgraph/raw/master/doc/processes-mnesia.png)
![processes-mnesia](https://github.com/psyeugenic/fgraph/raw/master/doc/processes-bigbang.png)


# Provisual v2.0 #

Yes a name change

## Getting Started v2.0 ##

Clone and Make

    git clone git://github.com/psyeugenic/fgraph.git
    git checkout master
    make

Start a new erlang node:

    erl -pa ebin -sname view

In the erlang shell type:

    provisual:start().

