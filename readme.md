# Clusterduck

#### A distributed computing library built atop of Jane Street's Rpc_parallel

This library provides the tools to create computational "topologies"
(DAGs) across a network, similar to the model that Apache Storm is based on.
The development of this framework was a response to the rather large
overhead/footprint of setting up Storm, relative to my humble needs.

In a nutshell, given a set of functions and their interdependencies,
Clusterduck will take care of the RPC plumbing required in order
to complete the computation using workers on multiple machines. 

## Workers

There exist two distinct types of workers: *spouts* and simple *workers*.
Spouts act as data sources, while workers process data from other upstream
workers and spouts.

For example, a possible topology might look like this:

    Spout A          Spout B
           \             /
            \           /
             \         /
              \       /
               \     /
                \   /
                 \ /
          Worker  C


Given the functionality for A, B and C, and the indication that C
depends on A and B, Clusterduck will set up RPC implementations
that call the function of worker C upon receiving a message from A or B.
If downstream workers from C existed, the result of the computation done
by worker C would be automatically sent to the immediate subworkers.

### Bundling data

A worker can simply process the data it receives from its dependencies
without any notion of relatedness between messages, or it can *bundle*
together messages that are considered to belong in the same batch. 

Each value sent over rpc has an associated sequence id, which is
used to determine the batches for bundled workers. The sequence id of a 
value is determined at spout level - a spout function must accept as
argument an `int -> 'a -> unit Deferred.t`, which is the *dispatcher*
passed to it. The dispatcher is used by a spout to relay messages to
subworkers. The dispatching for basic workers (i.e. non-spouts) is handled
behind the scenes.

The user is responsible for generating sequence ids in spouts. While a 
simple counter is probably suitable for most cases, naturally you can 
create arbitrary integer representations or hashes based on whichever
relation there is between multiple data points. 

Contrived example: 
Say someone wanted to build photo collages of pictures taken
at the same wall-time around the world. They could have a bunch of spouts
sending pictures each hour to a downstream worker, with the sequence id being
the current time at the location, e.g. 11. This way the downstream worker
will end up with bundles that contain the 11 o'clock image from each spout,
and these images can now be stitched together.

### Sequencing work

For bundled workers, the framework provides the option to process bundles/batches
one by one, in a strictly sequential order, i.e. the bundle with sequence
id `n` will only be processed after id `n - 1` has been processed.

## Debugging

The `Debugger` module can be used to create a debugger and pass it to 
`Builder.create`, so that the output of chosen workers is sent back to the
master and passed to a user defined function.

stdout and stderr are redirected to worker_name.out and worker_name.err 
files on the worker's machine in the directory passed to `Builder.launch`, 
which defaults to `/tmp/clusterduck`.
