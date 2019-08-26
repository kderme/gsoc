# Writing quickcheck-state-machine tests for rqlite

During my GSoC project for 2019, we decided to use property-based testing with [quicktest-state-machine](https://github.com/advancedtelematic/quickcheck-state-machine) for a distributed database, called [rqlite](https://github.com/rqlite/rqlite). In this blog post I discuss about writing a client for rqlite, writing the q-s-m test, using unix-processes to dynamically spawn rqlite nodes and then migrating to using docker instead.


## The client

As a first step I had to write a Haskell client for rqlite 
since it is actually written in Go language. I soon realised that the protocol rqlite uses is not well documented,
so writing a client would not be very easy. My first implementation had many bugs and the tests crashed very often. 
Fixing all these bugs that property-based testing discovered lead to a pretty robust client, which is now published on 
hackage (http://hackage.haskell.org/package/hs-rqlite).

The client uses http to communicate with the nodes and the nodes communicate through tcp with each other.
There are many things that can go wrong in a distributed system and properly handling all kind of errors that can appear is
crucial. For this reason I defined a always growing type for all the possible exceptions that could occur:

``` haskell
data RQliteError =
      NodeUnreachable IOError Int
    | StreamError ConnError
    | HttpError (Response String)
    | HttpRedirect (Response String)
    | MaxNumberOfRedirections [Response String]
    | FailedRedirection (Response String)
    | LeadershipLost Text
    | UnexpectedResponse String
    deriving (Show, Typeable, Exception)
```

It's interesting to see what are all the possible errors:
- We tried to reach a host, which didn't exist. In this case the `network` package throws a `IOException`, which is
reified as a `NodeUnreachable ..`.
- The tcp connection was succesfully created, but an error discupted the stream of data between the hosts (for example 
the connection was forcibly closed by the remote host). The `network` package throws `IOException` in this case,
but the `HTTP` package reifies them to `ConnError`, which is wrapped in `StreamError`.
- An internal error happened to the server, at the http protocol layer and the http response was not `OK`.
- We requested from a node which is not the leader of the cluster, for something that only the leader can reply. In this case the nodes redirect us to the leader. This is technicaly just another case of an http error (with error code 3xx), 
but since this is a distributed db, we decided that this error deserves its own constructor `HttpRedirect`. Our client
gives the option to make the redirection itself, so, in this case, this error should not appear to the user.
- By the time we query the leader, he may no longer be the reigning leader, so he will redirect us. We set a maximum
number of times this can happen (a bit rare though).
- It may also be that the http header which redirects us to a different location is missing (rare also).
- We requested something that only the leader can do. However there is currently no leader to redirect us. This can happen
when multiple nodes leave a cluster and there is no majority for a leader to be voted.
- We got a json which we cannot actually parse.
<br/>
Another possible error is that our SQLite query (actually rqlite is backed up by SQLite) returned an error, becasuse it 
failed to parse or we queried a table which doesn't exist. This error is not part of `RQliteError` because it is returned as a `Left` case.

The final error `UnexpectedResponse` was a very common error at the begining of the implementation. I soon realized that
in order to fix it, very descriptive comments should be provide about what exactly failed to parse and what we were
trying to do. With the feedback of testing I kept improving the parsers of the response, until there were no failures.

I used Aeson for the json parsing. Initially I had tried to define suitable types for each response and derive the `FromJSON` instance. But I soon realised that since the response can take multiple forms, I had to manually write parsers:

``` haskell
instance FromJSON PostResult where
    parseJSON j = do
        Object o <- parseJSON j
        case M.toList (o :: Object) of
            [("rows_affected", _), ("last_insert_id", Number n)] ->
                return $ PostResult $ base10Exponent n
            [("last_insert_id", Number n)] -> -- this happens when deleting
                return $ PostResult $ base10Exponent n
            [("error", String txt)] ->
                return $ PostError txt
            [] -> -- this happens when creating table
                return EmptyPostResult
            _ -> throw $ UnexpectedResponse $ concat
                    ["Failed to decode ", show j, " as PostResult"]
```

The client allows to very easily do read requests to the db (through http gets), write requests (through http posts), request
the state of the node, option to automatically redirect to the leader if we do a request to a follower. Read requests are
possible with all the consistency levels that rqlite provides:

``` haskell
data Level = None | Weak | Strong
        deriving (Show, Eq, Generic)
```

## The tests
Testing monadic code with `QuickCheck` is already possible using the `Test.QuickCheck.Monadic` module. Testing a 
distributed database tries to stretch this idea. `quickcheck-state-machine` makes this much easier, since we can model the
state of the db and apply preconditions and postconditions to this model. In our case the Model is:
``` haskell
data Model r = Model {
        dbModel :: DBModel
      , nodes   :: [(NodeRef r, Int)]
} deriving (Generic, Show)

data DBModel = DBModel {
      persons   :: [Person]
    , nodeState :: Map Int NodeState
    , cWhere    :: Int
    , cRef      :: Int
    } deriving (Generic, Show)

```

`persons` indicates the unique table that our db holds and which `Person` are currently inserted. `cWhere` indicates the physical location that a node runs (this can be a port, an ip address etc) and `cRef` is a unique identifier of nodes. `nodeState` shows the current state of the node: 

``` haskell
data NodeState = Running Int
               | Disconnected Int
               | Stopped Int
               | Paused Int
               | Done
               deriving (Show, Generic, ToExpr)
```
We will see below how, with the adoption of docker, we can change the state to Stopped or Disconnected and back to Running. `nodes :: [(NodeRef r, Int)]`
is a registry of all the running processes. In this [blog](https://github.com/kderme/gsoc/blob/master/blog/References.md) I write more about how q-s-m uses references, like this reference to the unix-process. At the end of each test, it is important that these processes are stopped. Doing proper cleanup is discussed in this [blog] (https://github.com/kderme/gsoc/blob/master/blog/cleanup.md).

The commands which transition the Model are:
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
a new node, since the node finds an existing log entry, instead of trying a new one, which will be probably out of date, since
other nodes will have continued. We also allow to insert a new Person in the database, Read the database Table and Delay for
a some amount of time. Trying different Delays is important, since rqlite uses many timeouts, like election-timeout,
heartbeat-timeout and leader-lease timeout. `Spawn` and `Respawn` have all the information needed to start a new node. `Level` at the `Get` constructor indicates the consistency level of our query. In another [blog](https://github.com/kderme/gsoc/blob/master/blog/rqlite-test.md), we will discuss about testing the consistency levels of rqlite.

## Using Docker
Injecting errors, like networks partitions, is an essential part of testing a distributed system. That's why we decided to change our tests from spawning unix-processes, to docker. The docker support of rqlite is still very early, but it was good enough for us. Docker gives a more flexible way to manipulate the networks and to disconnect nodes.

Disconnected node means that the node runs but no other node can reach it. In order to disconnect a node, I used the command
`docker network disconnect $container_name`
The initial issue I had, was that this command made the node unreachable also for http requests. So I decided I had to
connect each container on 2 networks: one to communicate with other containers and the other to make it available for 
requests. This is what I did to achieve this. First create two networks:
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
So now we can create a new container:
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
so that it is reachable for requests. The port on which the node communicated with other nodes doesn't need to be published.

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

## Summary
In this blog we discussed technical difficulties of testing a distributed databases and what needed to be done (writing a client, injecting errors, pausing nodes). In a future [blog](https://github.com/kderme/gsoc/master/blog/rqlite-test.md) we will discuss about testing the consistency of rlite, using the existing functionality.
