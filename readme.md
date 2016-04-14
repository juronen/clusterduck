# Clusterduck

#### A distributed computing library built atop of Jane Street's Rpc_parallel

This library provides the tools to create computational "topologies"
across a network, similar to the model that Apache Storm is based on.
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

### Sequencing work

For bundled workers, the framework provides the option to process messages
one by one, in a strictly sequential order, i.e. the message with sequence
id `n` will only be processed after id `n - 1` has been processed.
