# Deriving specification  from Property based testing

During my GSoC project for 2019, we decided to use property-based testing with quicktest-state-machine 
(https://github.com/advancedtelematic/quickcheck-state-machine) for a distributed database, called rqlite
(https://github.com/rqlite/rqlite). As a first step we had to write a Haskell client for rqlite 
since it is actually written in Go language. We soon realised that the protocol rqlite uses is not well documented,
so writing a client would not be very easy. Our first implementation had many bugs and the tests crashed very often. 
Fixing all these bugs that property-based testing discovered lead to a pretty robust client, which is now published on 
hackage (http://hackage.haskell.org/package/hs-rqlite). In this blog post we will discuss how black-box property-based testing can help us derive the missing
specification and reverse engineer a protocol, when documentation is lacking and also the difficulties of testing a
distributed system.

## The client

Our client uses http to communicate with the nodes and the nodes communicate through tcp with each other.
There are many things that can go wrong in a distributed system and properly handling all kind of errors that can appear is
crucial. For this reason we defined a always growing type for all the possible exceptions that could occur:

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
- We tried to reach a host, which didn't exist. In this case the `network` package throws a `IOException`, which we
reify as a `NodeUnreachable ..`.
- The tcp connection was succesfully created, but an error discupted the stream of data between the hosts (for example 
the connection was forcibly closed by the remote host). The `network` package throws `IOException` in this case,
but the `HTTP` package reifies them to `ConnError`, which we wrap in StreamError.
- An internal error happened to the server, at the http protocol layer and the http response was not `OK`.
- We requested a node which is not the leader of the cluster, for something that only the leader can reply. In this case the
nodes redirect us to the leader. This is technicaly just another case of an http error (with error code 3xx), 
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
failed to parse or we queried a table which doesn't exist. We don't actually have this error in `RQliteError` because we
don't throw it as an exception but return is as a `Left` case.

The final error `UnexpectedResponse` was a very common error at the begining of the implementation. We soon realized that
in order to fix it, we had to provide very descriptive comments about what exactly we failed to parse and what we were
trying to do. With the feedback of testing we kept improving our parsers of the response, until there were no failures.

Our Aeson Parsers look like this. Initially we had tried to define suitable types for each response and derive the `FromJSON` 
instance. But we soon realised that since the response can take multiple forms, we had to write our own parsers. 

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

Note that in order to spawn a node we create a new unix process using `createProcess` from the `process` package. It is also
important to kill all remaining processes at the end of the test. Otherwise, they may interfere with next tests. We have
found that doing proper cleanup of resources in `quickcheck-state-machine` is very tricky and it is an open issue how to
handle it in the general case https://github.com/advancedtelematic/quickcheck-state-machine/issues/335. What makes
this task hard is that `quickcheck-state-machine` takes some user defined functions, like `precondition`,
`transition`, `semantics` each of which may throw exceptions. In order to properly do any cleanup we should make sure to 
properly evaluate the result of these functions and catch any exception whenever used. If there is any exception, we should
find the current Model and do the cleanup. So the user should also provide a new `Model -> IO ()` function to clean the state.
The same should be applied to any `^C` user interrupt: It should be caught, do the cleanup and then exit. We recently fixed 
a bug, which caused `quickcheck-state-machine` to translate ^C interrupts to test failures 
https://github.com/advancedtelematic/quickcheck-state-machine/pull/320

Since this is not implemented yet, I found a workaround: At the end of each test, I return the final model and this is where
I do the cleanup. It works, but if there is some unexpected exceptions, the resources won't be cleaned up. 

## An expected test-case failure
<div id="container">
   ![Image description](https://github.com/kderme/gsoc/blob/master/blog/rqlite.png)
</div>

<center><img src="https://github.com/kderme/gsoc/blob/master/blog/rqlite.png"></center>
      <center>This is an image</center>
In this image we see a failing test, with the comment
<br/>
```
AnnotateC "PostconditionFailed \"PredicateC (Resp {getResp = Right (Got [])} :/= Resp {getResp = Right (Got [Person {name = \\\"Curry\\\", age = 37}])})\"" BotC
```
<br/>
and indeed we can see that we Got an empty list, while we would have expected a list with a single Perosn which was just
added (aka stale read). Let's see what happens here
- Node 0 is created, listening on Port 4001
- Node 1 is created, listening on Port 4003
- Node listening on 1, gets a post request, to insert a new Person. Since this node is not a leader, this request is 
redirected by the client to the node 0.
- Node 1 is requested to get all Persons. The consistency level is `None` so the node requests its local db (instead of
redirecting to other nodes). The previous insertion done on node 0 has not yet reached node 1, so we have a stale read.




  
