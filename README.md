# fgraph #

Some of these files already are in the reltool application. I originally wrote it just for fun but it ended up to show dependecies in reltool.


# Processes #

Processes is a process visualization tool. It can visualize the processeses and its ancestry in a system or the links between the processes. It will also show messages between processes.


## Getting Started ##

Start a new erlang node:

    erl -pa ebin -sname view

In the erlang shell type:

    processes:start().


A wxErlang window will appear and you can now connect to a remote node by clicking File in the meny and then Connect. Type in the remote node name, e.g.: mynode@hostname
