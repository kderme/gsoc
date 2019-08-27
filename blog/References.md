# References in quickcheck-state-machine

In this blog post we will discuss how references work in quickcheck-state-machine and some related bugs I fixed during my
GSoC project. This blog discusses internals of q-s-m and although having used q-s-m is not necessary it may help in 
understanding the content. We will follow the example in the [README](https://github.com/advancedtelematic/quickcheck-state-machine/blob/master/README.md), so familiarity with this helps.
[Here](http://www.well-typed.com/blog/2019/01/qsm-in-depth/) there is a more advanced example.

q-s-m is built on top of QuickCheck and uses one very important combinator of QuickCheck:
``` haskell
forAllShrinkShow :: Testable prop => Gen a -> (a -> [a]) -> (a -> String) -> (a -> prop) -> Property
```
This combinator captures the basic steps that a QuickCheck test follows: first we need a random Generator of a type a.
Then we need a way to shrink a to something smaller, in case the test fails and a way to print it. Finally a way to
run the test or create a Testable `prop` from a, that is something that can turn into a `Property`. Even when we write a
property like
``` haskell
reverse (reverse ls) === (ls :: [Int])
```
we still use a combinator similar to `forAllShrinkShow`. In this case, the type `a` is `[Int]`, the generator and the
shrinker are implied by the `Arbitrary` instance of [Int]. In the case of q-s-m, instead of a list of `Int`, the type
used is a list of Commands, the exact type of which is specified by the user. With the help of `Test.QuickCheck.Monadic`, 
the `a -> prop` part can test monadic code and q-s-m uses this to test the commands against a real system. 
It is important though that no random generation is done during the execution part, using functions like `pick`, or 
else shrinking and repetition of failed test is impossible. These are feautures q-s-m does not want to sacrifice. But this
creates a paradox: assume we test a db and the db gives ids to each new entry. How can we have a command which indicates that
this entry must be updated before we have even inserted the entry? Or if we test a filesystem, how can we have a command to
close a handle, before this handle is even created. Or if we want to test how `IORef` works, like in the example of
q-s-m [Readme](https://github.com/advancedtelematic/quickcheck-state-machine/blob/master/README.md), how do we generate a Command to write to an IORef, even before it exists?

The solution q-s-m gives to this is Variables and symbolic references. The commands, upon generation, do not contain 
the real handles or the real entry id or the real `IORef`, but Symbolic references:
``` haskell
Commands
  { unCommands =
      [ Command Create [ Var 0 ]
      , Command (Write (Reference (Symbolic (Var 0))) 5) []
      , Command (Read (Reference (Symbolic (Var 0)))) []
      ]
  }
```
The [ Var 0 ] indicates that this command creates a single reference, while the other commands don't create new references,
but use the existing one. At the stage of generation, nothing indicates that we test `IORef`. Variables are simple Ints,

``` haskell
newtype Var = Var Int

data Symbolic a where
  Symbolic :: Typeable a => Var -> Symbolic a

data Reference a r = Reference (r a)
```
that the generation code tries to keep them in increasing order, starting from 0 (although we will see how shrinking
disrupts this balance and caused one of the bugs we found). The real type becomes apparent from the semantics, which
return a Response which contains Concrete References.
``` haskell
data Concrete a where
  Concrete :: Typeable a => a -> Concrete a
```
Concrete is a simple wrapper of the real a. As you may have noticed we say that the user defined
cmd and resp contain references (Symbolic or Concrete), that is they have the structure of a container. Let's see the constraint that q-s-m requires for cmd and resp:
- For generation of commands, q-s-m requires a Foldable instance of Responses, so that it can extract the list of 
Variables (like the [ Var 0 ] we saw above. The Symbolic responses are created by a user defined function:
``` haskell
mock :: model Symbolic -> cmd Symbolic -> GenSym (resp Symbolic)
```
The GenSym monad is a state monad, where state is a counter.
- Execution requires more constraints.
Foldable resp is still needed, because we want to extract the list of Concrete
References this time. By assuming that the extracted References are on the same order, the execution keeps track of the
environment: that is a simple `Data.Map` of how symbolic vars map to Concrete values. This Environment helps reifying
cmd Symbolic to Concrete. To achieve this, execution traverse through the References of the Symbolic Commands and 
map each Symbolic to Concrete, until maybe one Symbolic is not found and then traversing stops. So, in addition a Traversable
instance of cmd is necessary. 

At this point we have discussed how most ingredients are created:
- cmd Symbolid: these are created by random generation.
- cmd Concrete: reified from cmd Symbolic, given the execution Environment.
- resp Symbolic: by the mock function.
- resp Concrete: returned by the semantics.

For shrinking the user can provide a shrinker for a single Commands, but q-s-m also tries to shrink the list of Commands, by discarding
whole commands. This is tricky though, because we must make sure that the references of the discarded cmd are not used.
That's why q-s-m tries to validate each shrunk list of Commands, before actually executing it. In this example:
``` haskell
Commands
  { unCommands =
      [ Command Create [ Var 0 ]
      , Command Create [ Var 1 ]
      , Command (Write (Reference (Symbolic (Var 1))) 5) []
      , Command (Read (Reference (Symbolic (Var 1)))) []
      ]
  }
```
the first command can be shrunk. The validation logic will also try to remap Commands, so that Variables start from 0 and
there are no gaps. In order to achieve this, the validation logic keeps track of an environment of 
remapped Variables `Map Var Var`, a counter and a Model which transitions with each command. 
Initially this counter is zero and the Map empty. When the validate logic runs mock for this command:
``` haskell
Command Create [ Var 1 ]
```
for counter=0 and extra the Variables of the response, it will be [ Var 0 ]. A mapping 0 -> 1 is kept in the environment
for future commands. Each command pass through this remapping of variables, so the final Commands is:
``` haskell
Commands
  { unCommands =
      [ Command Create [ Var 0 ]
      , Command (Write (Reference (Symbolic (Var 0))) 5) []
      , Command (Read (Reference (Symbolic (Var 0)))) []
      ]
  }
```
This traversal requires a Traversable instance for cmd, which as we mentioned is already needed for execution,
so this does not add a new constraint overall.

## The parallel case and the bugs.
In the previous part we discussed how q-s-m uses variables for the sequential case. q-s-m provides in addition the
possibility to test commands in parallel. We will discuss here about how shrinking works in the parallel case, following
an example and the bug I found and fixed.
Assume we have the following Parallel Command: the prefix is executed by one thread and then two threads executed the pair
in parallel. There is a barrier between the different pairs. Lets assume there is some failure in the semantics, 
so this parallel Command has to shrink.

``` haskell
ParallelCommands
  { prefix =
      Commands { unCommands =
            [ Command Create (Created (Reference (Symbolic (Var 0)))) [ Var 0 ]
            , Command Create (Created (Reference (Symbolic (Var 1)))) [ Var 1 ]
            ]
        }
  , suffixes =
      [  Pair
          { proj1 = Commands { unCommands = []}
          , proj2 = Commands
                { unCommands =
                    [ Command (Increment (Reference (Symbolic (Var 1)))) Incremented []]
                }
          }
      ,  Pair
          { proj1 = Commands { unCommands = [] }
          , proj2 =
              Commands
                { unCommands =
                    [ Command (Read (Reference (Symbolic (Var 1)))) (ReadValue 1) [] ]
                }
          }
      ]
      ]
  }
  ```
This example is pretty simple because the first thread actually executed no commands (empty list).
The first command can be shrunk, because the `Var 0` is never used. The validation for the parallel case uses the validation
of the sequential case and combines the results. The prefix will be remapped to:
``` haskell
prefix =
      Commands { unCommands =
            [ Command Create (Created (Reference (Symbolic (Var 0)))) [ Var 0 ]
            ]
        }
```
and the environment will acquire a remap 0 -> 1. After that, both proj1 and proj2 are validated with the new environment. 
The bug was in the way the model was advanced for the next Pair. It actually used a the Command before being remmaped, 
so it referenced a Variable which was never created.

## The second bug

The second bug was an open issue in q-s-m https://github.com/advancedtelematic/quickcheck-state-machine/issues/302 and it apppeared
from time to time in ci and threw a strange error which confused users. It was fixed with my [pr](https://github.com/advancedtelematic/quickcheck-state-machine/pull/315). This bug also affected me while 
trying to generalize parallelism in q-s-m: In this [pr](https://github.com/advancedtelematic/quickcheck-state-machine/pull/324) I extended parallelism from using strictly 2
threads, to using a user specified amount of threads. It was basically a generalization from using a Pair, to using a list. Before merging this
pr I was trying to add a sanity test, which tested the equivalence of the old implementation with the new one for n=2.
The tests kept failing and it was only when this bug was fixed that the tests passed. More details about this can be found [here](https://github.com/advancedtelematic/quickcheck-state-machine/pull/294#discussion_r272949230).

This bug did not have to do with shrinking, but with generation and execution.
A good question is, what is a valid parallel program, that we should allow to be executed? There is an
inherent uncertainty when running parallel programs: we can't actually know how the model advances when two streams of
commands are executed in parallel. So, during generation, q-s-m takes sequential commands and tests all permutation of 
the commands, to see if the preconditions of the user are still respected. It is also important that commands do not use 
references of other commands that are executed in parallel. Even though we don't know how the model advances, we should know
the environment created from each stream of execution (remember the environment is just a Map from Varibles to Concrete
elementes). The final environment is just the union of the two environment. But what if there is an uncertainty in the
number of concrete variables created (remember in order to create the environment we have to exctract the references
using the Foldable instance). Let's see an example where this can happen.

Assume a system where we can only create references:
``` haskell
data Command r
  = Create
```
The system allows only the first reference to be created and then fails with NotCreated.
``` haskell
data Response r
  = Created (Reference Int r)
  | NotCreated
```
We have a program which tries to create references in parallel:
``` haskell
      ParallelCommands
        { prefix = Commands { unCommands = [] }
        , suffixes =
            [ Pair
                { proj1 =
                    Commands { unCommands =
                          [ Command Create [ Var 0 ] ]
                      }
                , proj2 =
                    Commands { unCommands = 
                          [ Command Create [] ] 
                      }
                }
            ]
        }
```
Because of the inherent uncertainty of parallelism it is possible that proj2 will run before proj1, so the concrete
Response will contain a concrete element. As a result the length of the 2 responses, concrete and symbolic will not be the
same and the environment cannot be updated. This created a strange mockMismatch error, which was hard for the user to 
understand.

My fix was to add an additional check in the generation of commands. For each permutation we should not only check the
preconditions, but we should also make sure that the mocked responses all have the same amount of variables. This simple
fix made programs like the above to be discarded. The fix was added in this [pr](https://github.com/advancedtelematic/quickcheck-state-machine/pull/348).



