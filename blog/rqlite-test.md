In a previous blog we explain how we built a client for rqlite and some technical difficulties we faced while writing 
quickcheck-state-machine tests for it. Here we will discuss about injecting errors and testing the consistency of rqlite.
We believe our approach can be used to test other distributed systems.

## Using Docker

The first think we did was change our tests from using processes, to using docker. The docker support of rqlite is 
still very early, but it was good enough for us. Docker gives a more flexible way to manipulate the networks and to 
create parittions and it is easier to run from ci.

The Commands dsl is this one:

``` haskell
data Cmd node =
      Spawn Int Timeout Timeout Join
    | ReSpawn Int Int Timeout Timeout Join
    | Disconnect node
    | Connect node
    | Stop node
    | Insert node Person
    | Get node (Maybe Level)
    | Pause node
    | UnPause node
    | Delay Int
    deriving (Generic, Show, Functor, Foldable, Traversable)
```

It allows starting a node, stopping, pausing it, unpausing and restarting it. Restarting is different from starting
a new node, since the node finds an existing log entry, instad of trying a new one, which will be probably out of date, since
other nodes will have continued. We also allow to insert a new Person in the database, Read the database Table and Delay for
a some amount of time. Trying different Delays is important, since rqlite uses many timeouts, like election-timeout,
heartbeat-timeout and leader-lease timeout (more on this below). When we Read, we allow 3 different levels of consistency
(more on this below too).
``` haskell
data Level = None | Weak | Strong
        deriving (Show, Eq, Generic)
```

In the Model we keep the state of each node:
``` haskell
data NodeState = Running Int
               | Disconnected Int
               | Stopped Int
               | Paused Int
               | Done
               deriving (Show, Generic, ToExpr)
```
Disconnected means that the node runs but no other node can reach it. In order to disconnect a node, I used the command
`docker network disconnect $container_name`
The initial issue I had, was that this command made the node unreachable also for http requests. So I decided I had to
connect each container on 2 networks: one to communicate with other containers and the other to make it available for 
requests. So we first create two networks:
``` haskell
createRQNetworks :: IO ()
createRQNetworks = do
    -- this way we don't "polute" test output with messages (i.e. network
    -- may already be present).
    stdErr  <- openFile (testPath ++ "/" ++ "create_network_err") AppendMode
    void $ readCreateProcess (proc "docker"
        [ "network"
        , "create"
        , "--subnet"
        , "172.19.0.0/16"
        , "rqlite-http"]) {std_err = UseHandle stdErr}
        ""
    void $ readCreateProcess (proc "docker"
        [ "network"
        , "create"
        , "--subnet"
        , "172.18.0.0/16"
        , "rqlite-network"]) {std_err = UseHandle stdErr}
        ""
```
So now we can create a new container now:
``` haskell
  void $ readProcess "docker"
        [ "create"
        , "--net"
        , "rqlite-http"
        , "--ip"
        , ip
        , "-p"
        , show p ++ ":" ++ show p
        , "--name"
        , ndName
        , "rqlite/rqlite"
        , "-http-addr"
        , ip ++ ":" ++ show p
        , "-raft-addr"
        , ip' ++ ":" ++ show p'
        , "-raft-timeout"
        , show timeout
        , "-raft-election-timeout"
        , show elTimeout
        ] ""
```
and connect it to the second network
``` haskell
  void $ readProcess "docker"
        [ "network"
        , "connect"
        , "--ip"
        , ip'
        , "rqlite-network"
        , ndName
        ] ""
```
so now start the created node:
```
stdOut' <- openFile (testPath ++ "/out-" ++ show n) AppendMode
  stdErr' <- openFile (testPath ++ "/err-" ++ show n) AppendMode
  stdIn'  <- openFile "/dev/null" ReadMode
  -- we don't use readProcess here, because we don't want to block.
  void $ createProcess (proc "docker"
        [ "start"
        , "-a" -- this attaches the stdout and stderr to the one we specify.
        , ndName
        ]) {std_in = UseHandle stdIn', std_out = UseHandle stdOut', std_err = UseHandle stdErr'}
```
Note this is the only correct order to create a node on 2 networks. The `--net` option of `docker create` can only take
a single network. If `docker run` is used instead of `docker create` the container starts with a single networks and
fails to bind the http ip, so it's too late to later connect it to the second network.

When we create a container, the `-p` option is used to publish a port from the container to the host machine, 
so that it is reachablefor requests. The port on which the node communicated with other nodes need not be published.

Pausing and unpausig a container works like this:
``` haskell
readProcess "docker"
        [ "pause"
        , nodeName
        ] ""
```
``` haskell
readProcess "docker"
        [ "unpause"
        , nodeName
        ] ""
```
which basically sends a SIGSTOP and SIGCONT to all processes of the given container.


## Consistency

RQLite is a distributed database and is based on the go implementation of the raft protocol. Raft is a distributed
consistency protocol, with similar fault-tolerant guarantees to Paxos. The raft protocol ensures that a replicated 
state-machine is kept between different nodes, which for RQLite is a database. When a number of nodes are connected
an election is triggered and a leader is elected. When a request which changes the state reaches the leader, he has to
pass it through a quorum of nodes, before appending it to the log entry. Old entries are never changed.
The leader is also responsible to inform all followers for any changes and also send regular heartbeats to them. 
If a follower has not heard from the leader in a specific duration, he changes his state to candidate and starts 
an election.

The thing gets interesting when we start the discussion about reads. Only the leader should reply to reads.
Otherwise, it is very easy to get stale reads (we will discuss below abow tests that revealed this), so any consistency
is sacrificed.
Ideally we want reads to be fast, without having to access a quorum of nodes. Most raft-based systems provide different consistency levels and seems to be willing to sacrifice consistency for speed (in the default case). Before getting into details about the implementations, let's see what the protocol suggests. Raft actually allows multiple concurrent leaders. This can happen if for example a leader is cutoff from the network and other nodes elect a new leader. Older leaders should
not respond. But how does a leader knows there is another leader without trying to contact other nodes? The raft paper states:
```
a leader must check whether it has been deposed before processing a read-only request (its information may be stale if a more recent leader has been elected). Raft handles this by having the leader exchange heartbeat messages with a majority of the cluster before responding to read-only requests. Alternatively, the leader could rely on the heartbeat mechanism to provide a form of lease [9], but this would rely on timing for safety (it assumes bounded clock skew).
```
So a leader should either contact a quorum or if the mechanism of leader lease is used, it should check if its lease time
has expired.

All raft based implementations provide different levels of consistency for read operations: 
- Any node, leader or not, answers to read operations.
- The leader answers if his lease time has not expired. This depends on synchronized clocks.
- a stronger version where reads also must pass through a quorum of nodes, as a no-op operation.

Consul calls them stale/default/consistent. RQLite calls them none/weak/strong (weak is the default). Etcd, which actually depends on a  different implement      ations of raft, seems to be willig to sacrifice linearizability for sequential 
consistency (q-s-m does not test for sequential consistency). It still provides an option, not suggested to be used,
https://github.com/etcd-io/etcd/pull/866, which ensures linearizability by passing read operations from a quorum.

Timeout mechanism are used to ensure linearizability in the default modes for RQLite and Consul. The leader lease time has to be smaller than the heartbat timeout of followers. The raft implementations checks upon startup that within the same node, the leader lease timeout is smaller than the leader lease timoeut, so it ensures this property within one node. RQLite keeps the leader lease time fixed (500ms) and does not provide any option to change it. This ensures the wanted property across different nodes. Consul had tried to fix timeout related problems in the past, by reducing the leader lease time https://github.com/hashicorp/raft/commit/73bd785f4505fb27b97b253f37d40e4922d34227.
This wanted property ensures that a lease timeout expires before an elections starts on other nodes, even if the leader is partitioned and no concurrent leaders can occur. However this is easy to overcome. GC pauses or (as we did on
our tests) pausing and unausing the leader with SIGSTOP/SIGSTOP signals can create 2 concurrent leaders, since a paused node cannot hear of a expired timeout. In order to overcome even this case, the leader upon each get request compares the current time with the last time he heard from each follower. If enough leases
have expired, it responds with a Serivce Unavailable. The logic above is implemented in the checkLeaderLease function of
raft https://github.com/hashicorp/raft/blob/635796e5097fbfdb80f6f2d92abe66739a957380/raft.go#L853. The function time.now.Sub
finds the difference of times based on a monotonic clock. Monotonic clocks are better at checking the difference of two events than normal clocks and we found it very hard to mock them.





There are many other 

check if lease time has expired https://github.com/hashicorp/raft/blob/635796e5097fbfdb80f6f2d92abe66739a957380/raft.go
clock is monotonic https://golang.org/pkg/time/

provides 3 level of consistency: None, Weak, Strong.
