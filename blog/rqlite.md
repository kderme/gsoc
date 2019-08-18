# Deriving specification from Property based testing

During my GSoC project for 2019, we decided to use property-based testing with quicktest-state-machine 
(https://github.com/advancedtelematic/quickcheck-state-machine) for a distributed database, called rqlite
(https://github.com/rqlite/rqlite). As a first step I had to write a Haskell client for rqlite 
since it is actually written in Go language. I soon realised that the protocol rqlite uses is not well documented,
so writing a client would not be very easy. My first implementation had many bugs and the tests crashed very often. 
Fixing all these bugs that property-based testing discovered lead to a pretty robust client, which is now published on 
hackage (http://hackage.haskell.org/package/hs-rqlite). In this blog post we will discuss how black-box property-based testing can help us derive the missing specification and reverse engineer a protocol, when documentation is lacking and also the difficulties encountered while testing.

## The client

The client uses http to communicate with the nodes and the nodes communicate through tcp with each other.
There are many things that can go wrong in a distributed system and properly handling all kind of errors that can appear is
crucial. For this reason I defined a always growing type for all the possible exceptions that could occur:

```

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

```
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

## The tests
Testing monadic code with `QuickCheck` is already possible using the `Test.QuickCheck.Monadic` module. Testing a 
distributed database tries to stretch this idea. `quickcheck-state-machine` makes this much easier, since we can model the
state of the db and apply preconditions and postconditions to this model. In our case the Model is
```
data DBModel = DBModel {
      persons   :: [Person]
    , nodeState :: Map Int NodeState
    , cWhere    :: Int
    , cRef      :: Int
    } deriving (Generic, Show)

```

`persons` indicates the unique table that our db holds and which `Person` are currently inserted. `nodeState` shows if
the node is running or stopped (we make it possible to restart a node). `cWhere` indicates the physical location that a 
node runs (this can be a port, an ip address etc) and `cRef` is a unique identifier of nodes.

The commands which transition the Model are:
```
data Cmd node =
      Spawn Int Port Path Join
    | ReSpawn Int Int Port Path Join
    | Stop node
    | Insert node Person
    | Get node (Maybe Level)
```
`Spawn` and `Respawn` have all the information needed to start a new node. `Level` at the `Get` constructor indicates the
consistency level of our query. The stronger our consistency the less stale our read is.

## Cleanup

In order to spawn a node we create a new unix process using `createProcess` from the `process` package. It is also
important to kill all remaining processes at the end of the test. This is an expected and more general issue, since we test monadic code and not pure (which is what QuickTest is usually used for) and it affects many users of q-s-m https://github.com/advancedtelematic/quickcheck-state-machine/issues/335, usually resulting in ad-hoc solutions. We recently managed to provide an adequate solution for this issue https://github.com/advancedtelematic/quickcheck-state-machine/pull/346. Cleanup is necessary in the following cases: 

- A test finishes. Before other tests start the cleanup must be performed, so that they find an empty state.
- A test failed and shrinking starts. Again it must find an empty state.
- A test threw an exception in the middle of execution. Exceptions handlers must be placed, in order to do the cleanup before shrinking starts.
- An async exception interrupted a test. I recently made q-s-m not shrink when async exceptions are encountered. https://github.com/advancedtelematic/quickcheck-state-machine/pull/320. This is the correct approach, since async exceptions indicate that the user wants to end the tests and not that a test failed. However, even in this case some cleanup must be performed, before exiting. So our handlers must run with async-exceptions masked.
- Test results are undeterministic, like in the parallel case, so tests are run multiple times each looking for race conditions. Each execution must be performed on an empty state.
- Cleanup can be used to make tests faster. For example if each test starts a db at the begining, we may want to just start the db before the first test and use the cleanup to clean any entries of the db. By doing so, time is not spent to stop and restart the db each time. In some of our tests. we used this to make sure that each tests starts from a specific topology, without the need to kill all nodes and restart them each time, which takes a lot of time.

After some discussions, we ended up that the cleanup shoud be a user defined function `Model Concrete -> IO ()`. This would be the easiest for the user, since he is familiar with what the Model is. But where exactly should this cleanup be done and which is the correct model (the model keeps transitioning during the execution). Cleanup is getting even more hard in the parallel case, where parallelism creates a undeterministic Model. In order to answer these questions we should discuss about how q-s-m works. q-s-m is built on top of QuickCheck and uses the same 3 steps: random generation, execution, shrinking. Cleanup, is of-course something that only affects execution. During execution q-s-m uses a TChan to store history of events like the start of an execution of a Command or the end of its execution. In the sequential case, the history is pretty simple:
[Invocation.., Response.., Invocation.., Response.., Invocation.., Response..] and it is very easy to see the order of actions, and create the final Model. Even if some exception happens, we can recreate the current Model and run the cleanup. But in the parallel case history can be more interesting:
[Invocation{pid = 0}.., Response{pid = 0}.., Invocation {pid = 0}.., Invocation {pid = 1}.., Response{pid = 1}.., Response{pid = 0}..]
We don't actually know the real order of execution of commands. A real order may not even exist, this is specified by a later linearizability check (we will discuss more about this on a different blog). We only use here the first step of linearizability checker: create a Tree of all the possible interleaving. For our case above the Tree would look like:
`Node (Operation {pid=0}) [Node Operation {pid=0} [Node Operation {pid=1}], Node (Operation {pid=1}) [Node Operation {pid=0}]]`
or in a better way
```
Node Operation {pid=0}
|
|_ Node Operation {pid=0} .. 
|  |
|  |_ Node Operation {pid=1} .. 
|
|_ Node Operation {pid=1} .. 
   |
   |_Node Operation {pid=0} .. 
```
So, we can't know what the real final Model is. Our solution is to decide that we don't actually need to know the real Model, any Model will do, so we just pick a path of the Tree. After all, in all our examples, resources, like unix processes, that need cleanup are kept in containers with foldable structure. Even if the unix processes are created in different order, the result will be the same structure. In a later pr, I added checks that enforce a specific number of resources each commands creates, which helps even more, this assumption.

## An expected test-case failure

<center><img src="https://github.com/kderme/gsoc/blob/master/blog/rqlite.png"></center>
      <center>This is an image</center>
In this image we see a failing test, with the comment
<br/>

```
AnnotateC "PostconditionFailed \"PredicateC (Resp {getResp = Right (Got [])} :/= Resp {getResp = Right (Got [Person {name = \\\"Curry\\\", age = 37}])})\"" BotC
```
<br/>
and indeed we can see that we Got an empty list, while we would have expected a list with a single Perosn which was just
added (aka stale read). Let's see what happens here:

- Node 0 is created, listening on Port 4001
- Node 1 is created, listening on Port 4003
- Node listening on 1, gets a post request, to insert a new Person. Since this node is not a leader, this request is 
redirected by the client to the node 0.
- Node 1 is requested to get all Persons. The consistency level is `None` so the node requests its local db (instead of
redirecting to other nodes). The previous insertion done on node 0 has not yet reached node 1, so we have a stale read.




  
