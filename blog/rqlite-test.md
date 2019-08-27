# Testing rqlite

In a previous [blog](https://github.com/kderme/gsoc/blob/master/blog/rqlite.md) we explain how we built a client for rqlite and some technical difficulties we faced while writing 
[quickcheck-state-machine](https://github.com/advancedtelematic/quickcheck-state-machine) tests for it. Here we will discuss about testing the consistency of rqlite.
We believe our approach can be used to test other distributed systems, with consistency guarantees.

## Raft Consistency

Rqlite is a distributed database and is based on the go implementation of the raft protocol. Raft is a distributed
consistency protocol, with similar fault-tolerant guarantees to Paxos. The raft protocol ensures that a replicated 
state-machine is kept between different nodes. When a number of nodes are connected
an election is triggered and a leader is elected. When a write request reaches the leader, he has to
pass it through a quorum of nodes, before appending it to the distributed log entry. Old entries are never changed.
The leader is also responsible to inform all followers for any changes and also send regular heartbeats to them. 
If a follower has not heard from the leader in a specific duration, he changes his state to candidate and starts 
an election.

The thing gets interesting when we start the discussion about reads. Only the leader should reply to reads.
Otherwise, it is very easy to get stale reads (we will discuss below abow tests that revealed this).
Ideally we want reads to be fast, without having to access a quorum of nodes. Most raft-based systems provide different consistency levels and seem to be willing to sacrifice consistency for speed (in the default case). Before getting into details about the implementations, let's see what the protocol suggests. Raft actually allows multiple concurrent leaders. This can happen if for example a leader is cutoff from the network and other nodes elect a new leader. Older leaders should
not respond. But how does a leader knows there is another leader without trying to contact other nodes? The raft paper states:
```
a leader must check whether it has been deposed before processing a read-only request (its information 
may be stale if a more recent leader has been elected). Raft handles this by having the leader exchange 
heartbeat messages with a majority of the cluster before responding to read-only requests. Alternatively, 
the leader could rely on the heartbeat mechanism to provide a form of lease [9], but this would rely on 
timing for safety (it assumes bounded clock skew).
```
So the paper allows opion: a leader should either contact a quorum or if the mechanism of leader lease is used, it should check if its lease time has expired.

The raft implementations provide also different levels of consistency for read operations: 
- Any node, leader or not, answers to read operations.
- The leader answers if his lease time has not expired. This depends on synchronized clocks.
- a stronger version where reads also must pass through a quorum of nodes, as a no-op operation.

Rqlite calls them none/weak/strong (weak is the default). Consul (a different system based on the same raft implementation as rqlite) calls them stale/default/consistent. Etcd (which actually depends on a  different implementations of raft) seems to be willing to sacrifice linearizability for sequential 
consistency (q-s-m does not test for sequential consistency). It still provides an option, not suggested to be used,
https://github.com/etcd-io/etcd/pull/866, which ensures linearizability by passing read operations from a quorum.

As the paper suggests, timeout mechanism are used to ensure linearizability in the default modes for rqlite and Consul. The important property needed is that the leader lease time has to be smaller than the heartbat timeout of followers. If this property holds no concurrent leaders can occur. The raft implementations checks upon startup that within the same node, the leader lease timeout is smaller than the heartbeat timoeut, so it ensures this property within one node. Rqlite keeps the leader lease time fixed (500ms) and does not provide any option to change it. This ensures the wanted property across different nodes. Consul had also tried to fix timeout related problems in the past, by reducing the leader lease time https://github.com/hashicorp/raft/commit/73bd785f4505fb27b97b253f37d40e4922d34227.

This wanted property ensures that a lease timeout expires before an elections starts on other nodes, even if the leader is partitioned and no concurrent leaders can occur. However this is easy to bypass. GC pauses or (as we did on
our tests) pausing and unausing the leader with SIGSTOP/SIGSTOP signals can create 2 concurrent leaders, since a paused node cannot hear an expired timeout. In order to overcome even this case, the leader upon each get request compares the current time with the last time he heard from each follower. If enough leases
have expired, it responds with a Serivce Unavailable. The logic above is implemented in the checkLeaderLease function of
raft https://github.com/hashicorp/raft/blob/635796e5097fbfdb80f6f2d92abe66739a957380/raft.go#L853. The function time.now.Sub
finds the difference of times based on a monotonic clock. Monotonic clocks are better at checking the difference of two events than normal clocks and we found it very hard to mock them. So by using this case, even if there are two concurrent leaders, one leader is just a leader on paper, since his lease time has expired and he will find out on the next query. Below we will explain the above with more concrete examples.

## a test-case failure with consistency level None

<center><img src="https://github.com/kderme/gsoc/blob/master/blog/rqlite.png"></center>
      <center></center>
In this image we see a failing test, with the comment
<br/>

```
AnnotateC "PostconditionFailed \"PredicateC (Resp {getResp = Right (Got [])} :/= Resp {getResp = Right (Got [Person {name = \\\"Curry\\\", age = 37}])})\"" BotC
```
<br/>
This test case, that random test disovered, shows a linearizability checker failure, beause we have a stale read:
read returns an empty list, while we would have expected a list with a single Perosn which was just
added. Let's see what happens here:

- Node 0 is created, listening on Port 4001
- Node 1 is created, listening on Port 4003
- Node listening on 1, gets a post request, to insert a new Person. Since this node is not a leader, this request is 
redirected by the client to the node 0.
- Node 1 is requested to get all Persons. The consistency level is `None` so the node requests its local db (instead of
redirecting to other nodes). The previous insertion done on node 0 has not yet reached node 1, so we have a stale read.

## Testing weak consistency
As we discussed above, with proper injected error, the weak consistency of rqlite can have stale reads. So I tried to trigger it.

First and foremost, in order to trigger this stale read it is important at some point, an election is triggered on a stable network (where a leader already exists) and a new leader is elected. However this should happen without killing the existing leader (as the existing leader has to be alive to give us the stale read). 

The first thing we tried was to exploit an issue with the [raft implementation](https://github.com/hashicorp/consul/issues/1674) (which should actually still be an open issue). According to this issue, killing and restarting a node can cause an election on a stable network. So we updated our test, which at the time was using unix-processes, with two commands: kill and respawn. When these commands were executed on the model, it changed its state to unavailable. We also added a precondition, to never query unavailable nodes, as this would result in timeout. Tests cases like this appeared:
spawn 3 nodes
wait to elect a leader
kill 1 node
respawn the node
wait election to finish
write to the new leader (the respawned node)
read from the old leader.

The election was indeed trigered, but not the stale read. The old leader realized it's not the leader anymore and redirected to the new leader instead of responding with old data. We also tried to split the commands in different thread/clients, but again the stale read didn't appear.

So killing and respawning the node was not enough. We then added commands to partition nodes from the network (for this we migrated to docker) test cases like this appeared:
spawn 3 nodes
partition the leader
wait for election from the other 2 nodes
write to the new leader
read from old leader

What happened was a `NotLeader` error from the old leader. In this case the timeouts ensure that the leader lease time expires before the other nodes decide to elect a new leader.

So we thought that maybe we could try to trigger the election a bit faster. We tested test scenarios like this:
spawn 3 nodes
partition a follower
wait for its heartbeat timeout to end
partition the leader
reconnect the follower (he now straight away triggers an election)
wait for election from the other 2 nodes
write to the new leader
read from old leader

Unfortunately the election from the two nodes always takes more than 500ms. We are not quite sure why this happens, but
no option we tried made the election faster. The old leader didn't respond with the stale read, but instead returned NotLeader.

Since, the leader lease timeouts, we thought that it would help if we actually pause the old leader, so that it does not hear any timeout. So we added pause and resume on our tests:
spawn 3 nodes
partition a follower
wait for its heartbeat timeout to end
partition the leader
pause the leader
reconnect the follower (he now straight away triggers an election)
wait for election from the other 2 nodes
write to the new leader
continue the old leader
read from old leader

Unfortunately the old leader still returns NotLeader. This happens because, as we mentioned, even if the leader is paused for some time, when requested he checks the current time and compares it with the time of last interactions. The time is given form a monotonic clock. We figured out that the only way to trigger this stale read is to actually mock this function https://golang.org/pkg/time/ of go, into returning a skewed time.

Work on this is still done, but some difficulties are encountered: Our tests are based on docker. The docker containers take their time form the clock of the host, so we can't just change the clock of a container. Virtualization, that Jepsen tests use, provide more flexibility in this regard, but they be harder to set up: docker can be easily be used in ci.

Another library called [libfaketime](https://github.com/wolfcw/libfaketime) can be used to mock system calls related to time. This [stackoverflow thread](
https://stackoverflow.com/questions/29556879/is-it-possible-change-date-in-docker-container) explains how to use it through docker. However as people point out, this is not supposed to work for statically linked libraries, like go lang. Using a custom compiled version of rqlite through docker is something we're trying next. It seems though this is the final piece remaining to triggering the stale read.

## Summary
In this blog we discussed about the consistency levels of rqlite and how we used q-s-m tests to hunt down inconsistencies. We thing we narrowd down the stale read of the weak consistency as much as possible to mocking a single system-call. If you have any suggestions on how to mock this function https://golang.org/pkg/time/, for an system running on docker, I would be interested to know <k.dermenz@gmail.com>.


## References
[1] In Search of an Understandable Consensus Algorithm (Extended Version) Diego Ongaro and John Ousterhout Stanford University
