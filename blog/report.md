## GSoC-2019 report

Here I keep a log of all my work for my gsoc-2019 project https://summerofcode.withgoogle.com/projects/#5114362358923264. I also use this file as the final report to Haskell.org.

I wrote many q-s-m tests for persistent
https://github.com/kderme/gsoc
I used this test suite to find bugs or missing features in q-s-m itself and in persistent <br/>
https://github.com/yesodweb/persistent/pull/915 (merged and released) <br/>
https://github.com/yesodweb/persistent/pull/922 (merged and released) <br/>
https://github.com/yesodweb/persistent/pull/945 (merged and released) <br/>
https://github.com/yesodweb/persistent/issues/948 (issue reported)

I generalized parallelism in q-s-m to an arbitrary number of threads (was limited to 2)
https://github.com/advancedtelematic/quickcheck-state-machine/pull/324
The same pr also introduced many meta-properties for the new shrinker of q-s-m.
In the same pr I gave the possibility to draw automatically plots of failing tests with graphviz-dot, like portrayed here 
https://github.com/advancedtelematic/quickcheck-state-machine/issues/316#issuecomment-504011309.
At the same link I had a discussions with Stevan, about linearizability and how we can extend it.

I have implemented a new way to interact with SQLite from multiple threads in Haskell. In order to implement this I wrote a new library
https://github.com/kderme/async-queue
which I plan to publish at hackage. The implementation is adopted in persistent. The fork can be found here 
https://github.com/kderme/persistent.
I have written a pretty big blog post which explains what I did and other related interesting stuff:
https://github.com/kderme/gsoc/blob/master/blog/blog-sqlite.md
A subset of this work above and the persistent test is merged as a q-s-m test https://github.com/advancedtelematic/quickcheck-state-machine/pull/349.

I fixed some q-s-m bugs. Some of them had affected users in the past.
This bug was very subtle and I had spent many days hunting it down:
https://github.com/advancedtelematic/quickcheck-state-machine/pull/315
I also fixed a bug which made q-s-m being unresponsive on ^C (I learned many new things while working on it about exceptions handling and async-exceptions)
https://github.com/advancedtelematic/quickcheck-state-machine/pull/320
I hunted down and found the cause of a bug which existed for long in q-s-m and made tests sometimes crash:
https://github.com/advancedtelematic/quickcheck-state-machine/issues/318
The bug was also reported in the past https://github.com/advancedtelematic/quickcheck-state-machine/issues/311
and it is now fixed.
https://github.com/advancedtelematic/quickcheck-state-machine/pull/319 (Stevan did this pr and I reviewed it).

I also came up with a shrinking optimization which can be used pretty well in q-s-m and I believe it can significantly reduce shrinking time
which is a big bottleneck. I explain it in many details here
https://github.com/advancedtelematic/quickcheck-state-machine/issues/321
I have also implemented it in this branch https://github.com/advancedtelematic/quickcheck-state-machine/tree/quick-shrink-monadic
It's a bit experimented, so I haven't opened a pr yet. The implementation touched many internal structures of QuickCheck which are not meant to be used (although fortunately QuickCheck exports them, so implementing this idea in q-s-m does not require any changes from QuickCheck). However, since the idea could have many other use cases (not only related to q-s-m) I also asked the opinion from people of QuickCheck:
https://github.com/nick8325/quickcheck/issues/263

A small release issue I found while using this library
https://github.com/tweag/pthread/pull/4

I added a new q-s-m example, called Overflow, which fails only when there are >3 threads https://github.com/advancedtelematic/quickcheck-state-machine/pull/325

I discussed with Stevan issue (https://github.com/advancedtelematic/quickcheck-state-machine/issues/326), which is related to  how to handle exceptions and whether testing should continue when they arise. Lead to pr https://github.com/advancedtelematic/quickcheck-state-machine/pull/331.

With many small prs I tried to fix existing tests, which often made CI fail (discussed at https://github.com/advancedtelematic/quickcheck-state-machine/issues/327). Lead to prs 
- https://github.com/advancedtelematic/quickcheck-state-machine/pull/328 (which fixes some random generators), 
- https://github.com/advancedtelematic/quickcheck-state-machine/pull/330 (some bug in tests),
- https://github.com/advancedtelematic/quickcheck-state-machine/pull/337 (an exception from graphviz). Related to the last pr, I also fixed a small bug in graphviz https://github.com/ivan-m/graphviz/pull/45 (not merged yet).

One pr (https://github.com/advancedtelematic/quickcheck-state-machine/pull/339) which fixes the labeling of q-s-m (which was a bit verbose) and replaces the default output, without any breaking api changes. Related pr which also improved the output of tests https://github.com/advancedtelematic/quickcheck-state-machine/pull/341. Other related issues and work in progress: https://github.com/advancedtelematic/quickcheck-state-machine/issues/342, https://github.com/advancedtelematic/quickcheck-state-machine/issues/340

With Stevan we decided to test rqlite, a distributed database. To make this possible, I wrote a Haskell client for rqlite and published it on Hackage (http://hackage.haskell.org/package/hs-rqlite). Using this client I wrote a pretty big test https://github.com/advancedtelematic/quickcheck-state-machine/pull/350. It uses various injected errors, like killing and respawning nodes,disconnecting and reconnecting, pausing and unpausing processes.
This https://github.com/kderme/gsoc/blob/master/blog/rqlite.md has more technical details
(writing a rqlite client, using unix-processes then moving to docker, injecting errors etc) and another blog https://github.com/kderme/gsoc/blob/master/blog/rqlite-test.md about the tests themselves and the consistency
levels of rqlite. 

I recently added a new function in q-s-m, which takes care of cleaning up the state after the end of each test.
The pr https://github.com/advancedtelematic/quickcheck-state-machine/pull/346 also has a a new q-s-m example with many 
meta-properties, that is it tests q-s-m itself. An extensive discussion about it can be found here https://github.com/advancedtelematic/quickcheck-state-machine/issues/335. I also wrote a blog post about it https://github.com/kderme/gsoc/blob/master/blog/cleanup.md which explains how cleanup is done, why it's necessary and why its
implementation is interesting for the parallel case of q-s-m. 

I fixed a bug in q-s-m https://github.com/advancedtelematic/quickcheck-state-machine/pull/348 which made some of my tests
fail. I also wrote another q-s-m test, which fails without the fix (same pr). In this blog https://github.com/kderme/gsoc/blob/master/blog/References.md I discuss about some internals of q-s-m and some of my bug fixes in q-s-m, included this one.

I wrote a meta-blog https://github.com/kderme/gsoc/blob/master/blog/meta-blog.md which introduces all my blogs 
(which I have mentioned above) in a less descriptive way. So it's not a report, like this file, but is intended for someone
outside Haskell.org who would like to read about my work.

Feedback for my work and my blogs is very welcome <br/>
Thanks <br/>
Kostas Dermentzis <br/>
<k.dermenz@gmail.com>
