In a previous blog we explain how we built a client for rqlite and some technical difficulties we faced while writing 
quickcheck-state-machine tests for it. Here we will discuss about injecting errors and testing the consistency of rqlite.
We believe our approach can be used to test other distributed systems.

The first think we did was change our tests from using processes, to using docker. The docker support of rqlite is 
still very early, but it was good enough for us. Docker gives a more flexible way to manipulate the networks and to 
create parittions, than simple processes.

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
        
