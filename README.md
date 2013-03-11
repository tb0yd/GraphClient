GraphClient
===========

Musings about using any data base as a graph database through a graph client

Description

At ErlangDC a group was formed to look in to using riak core to imnplement a distributed graph database using the pregel algorythm.  The pregel algorythm models the graph as a single set of vertices which contain their outgoing edges rather than a set of vertices and a separate set of edges.  Many of the calculations are broken in to "Super-steps" so that the vertices can complete a super step by first receiving messages from other vertices in the proceeding step, then doing any required modification of their state and finally sending out any resultant messages for the next Superstep.  Thus no vertex has to access another during a superstep.

For example, suppose we are doing a shortest path calculation.  psudo code for a single node during a single step might be;

1.  receive a message that has the source startID, endID and the path-so-far.
2.  if (self() == endID) send the path back to the client,
3.  else attach self() to the end of the path and send it to every outgoing edge

So let's say a vertex "D" gets a message {start: "A", end: "K", [C,E,B,A]}.
"D" says "I am not 'K'".  Then it sends  {start: "A", end: "K", [D,C,E,B,A]} to all of its outgoing edges.  

eventually either "K" will respond with a path or no more messages will be sent, meaning "you can't get there from here".

There is a lot of vertex to vertex communication so it would be better to get the band-width that a p2p system would allow with riak core.  However, there is a lot of work to do on the backend.  work already included in a database.

With that in mind, a similar calculation can be accomplished by writing a custom client that manages the supersteps on top of an existing database.  In this case the the client would send out messages and then the messages for the superstep would all be returned to the client which would then resend them at the beginning of the next step.  twice the number of message sends and all through the one client, but I suspect that for most problems it wouldn't matter.  

Architecture Speculation

Currently imagining a client that would receive a query which may include several operations.  The client would parse the query and send the operations the first one to the graph_calculator.  The graph_calculator would prepare the messages for the first superstep and then spawn one worker process for each message.  The worker would pull the vertex state from the database, modify the state if needed, assemble any outgoing messages, save the new state and return the messages to the graph_calculator.  The graph_calculator would either stop the operation or initiate the next superstep. When the operation is finished the result would be returned to the client which would either use that as input for the operation or return the results of the query.