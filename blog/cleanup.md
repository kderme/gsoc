
## Cleanup

In a previous [blog](https://github.com/kderme/gsoc/edit/master/blog/rqlite.md), 
we discussed about a q-s-m test for rqlite, which spawns and keeps a registry of new unix-processes.
It is also important to kill all remaining processes at the end of the test. This is an expected and more general issue, 
since we test monadic code and not pure (which is what QuickTest is usually used for) and it 
[affects](https://github.com/advancedtelematic/quickcheck-state-machine/issues/335)
many users of q-s-m, usually resulting in ad-hoc solutions. 
We recently managed to provide an adequate solution for this issue in this 
[pr](https://github.com/advancedtelematic/quickcheck-state-machine/pull/346). Cleanup is necessary in the following cases: 

- A test finishes. Before other tests start the cleanup must be performed, so that they find an empty state.
- A test failed and shrinking starts. Again it must find an empty state.
- A test threw an exception in the middle of execution. Exceptions handlers must be placed, in order to do the cleanup before 
shrinking starts.
- An async exception interrupted a test. In this [pr](https://github.com/advancedtelematic/quickcheck-state-machine/pull/320)
I made q-s-m not shrink when async exceptions are encountered. This is the correct thing to do, since async exceptions 
indicate that the user wants to end the tests and not that a test failed. However, even in this case some cleanup must be 
performed, before exiting. So our handlers must run with async-exceptions masked.
- Test results are non-deterministic, like in the parallel case, so tests are run multiple times each looking for race 
conditions. q-s-m provides this functionality through the runParalelNTimes command. However, it was impossible for users
to cleanup the state after each execution, which made these functions impossible to use for many cases.
- Cleanup can be used to make tests faster. For example if each test starts a db at the begining, we may want to just start 
the db before the first test and use the cleanup to clean any entries of the db. By doing so, time is not spent to stop and 
restart the db each time. In some of our tests. we used this to make sure that each tests starts from a specific topology, 
without the need to kill all nodes and restart them each time, which takes a lot of time.

After some discussions, we ended up that the cleanup shoud be a user defined function `Model Concrete -> IO ()`. This would 
be the easiest for the user, since he is familiar with what the Model is. But where exactly should this cleanup be done and 
which is the correct model (the model keeps transitioning during the execution). Cleanup is getting even more hard in the 
parallel case, where parallelism creates a non-deterministic Model, that is we don't know what the final model is. 

In order to answer these questions we should discuss about 
how q-s-m works. q-s-m uses a TChan to store history of events 
like the start of an execution of a Command or the end of its execution. In the sequential case, the history is pretty 
simple like:

`[Invocation, Response, Invocation, Response, Invocation, Response]` 

and it is very easy to see the order of actions, and create the final Model. Even if some exception happens, we can recreate
the current Model and run the cleanup. But in the parallel case history can be more interesting:

`[Invocation{pid = 0}, Response{pid = 0}, Invocation {pid = 0}, Invocation {pid = 1}, Response{pid = 1}, Response{pid = 0}]`

We don't actually know the real order of execution of commands. A real order may not even exist, this is specified by a 
later linearizability check (we will discuss more about linearizability tests on a different 
[blog](https://github.com/kderme/gsoc/blob/master/blog/rqlite-test.md). We only use here the first step of 
linearizability checker: create a Tree of all the possible interleaving. For our case above the Tree would look like:

`Node (Operation {pid=0}) [Node Operation {pid=0} [Node Operation {pid=1}], Node (Operation {pid=1}) [Node Operation {pid=0}]]`

or in a better way
```
Node Operation {pid=0}
|
|_ Node Operation {pid=0}
|  |
|  |_ Node Operation {pid=1}
|
|_ Node Operation {pid=1} 
   |
   |_Node Operation {pid=0}
```
So, we can't know what the real final Model is. Our solution is that we don't actually need to know the real Model,
any Model will do, so we just pick a path of the Tree. Paths of a Tree can be found with a simple function:

``` haskell
operationsPath :: Forest (Operation cmd resp) -> [Operation cmd resp]
operationsPath = go []
    where
      go :: [a] -> Forest a -> [a]
      go acc [] = reverse acc
      go acc (Node a f : _) = go (a:acc) f
```

After all, in all our examples, resources, like unix processes, that
need cleanup are kept in containers with foldable structure. the order of insertion not that important,
even if the unix processes are created in different order, 
the result will be the same structure. In a later pr, I added checks that enforce a specific number of resources each 
commands creates, which helps even more, this assumption. 
