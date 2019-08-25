I wrote many q-s-m tests for persistent
https://github.com/kderme/gsoc
I used this test suite to find bugs or missing feautures  q-s-m itself, also some bugs in persistent
https://github.com/yesodweb/persistent/pull/915 (merged and released)
https://github.com/yesodweb/persistent/pull/922 (merged and released)
https://github.com/yesodweb/persistent/pull/945 (merged and released)
https://github.com/yesodweb/persistent/issues/948 (issue reported)


I generalized paralellism in q-s-m to an arbitrary number of threads (was limited to 2)
https://github.com/advancedtelematic/quickcheck-state-machine/pull/324
The same pr also introduced many meta-properties for the new shrinker of q-s-m.
In the same pr I gave the possibility to draw automatically plots of failing tests with graphviz-dot, like portayed here 
https://github.com/advancedtelematic/quickcheck-state-machine/issues/316#issuecomment-504011309
At the same link I had a discussions with Stevan, about linearizability and how we can extend it.

I have implemented a new way to interract with SQlite from Haskell. In order to implement this I wrote a new library
https://github.com/kderme/async-queue
which I plan to publish at hackage. The implementation is adopted in persistent. The fork can be found here 
https://github.com/kderme/persistent
I have written a pretty big blog post (not posted yet) which explains what I did and other relate intersting stuff:
https://github.com/kderme/gsoc/blob/master/blog/sqlite.pdf

I fixed some q-s-m bugs. We found that some of them had affected many users in the past.
This bug was very subtle and I had spent many days hunting it down:
https://github.com/advancedtelematic/quickcheck-state-machine/pull/315
I also fixed a bug which made q-s-m being unresponsive on ^C. I learned many things while working on this bug about exceptions handling and async-exceptions which I didn' know.
https://github.com/advancedtelematic/quickcheck-state-machine/pull/320
I hunted down and found the cause of a bug which existed for long in q-s-m and made tests sometimes crash:
https://github.com/advancedtelematic/quickcheck-state-machine/issues/318
The bug was also reported in the past https://github.com/advancedtelematic/quickcheck-state-machine/issues/311
and it is now fixed.
https://github.com/advancedtelematic/quickcheck-state-machine/pull/319 (Stevan did this pr and I reviewed it).

I also came up with a shrining optimization which can be used pretty well in q-s-m and I believe it can significantly reduce shrinking time
which is a big bottleneck. I explain it in many details here
https://github.com/advancedtelematic/quickcheck-state-machine/issues/321
I have also implemented it in this branch https://github.com/advancedtelematic/quickcheck-state-machine/tree/quick-shrink-monadic
It's a bit experimented, so I haven't opened a pr yet. The implementation touched many internal structures of QuickCheck which are not meant to be used (although fotunately QuickCheck exports them, so implementing this idea in q-s-m does not require any changes from QuickCheck). However, since the idea could have many other use cases (not only related to q-s-m) I also asked the opinion from people of QuickCheck:
https://github.com/nick8325/quickcheck/issues/263 (no response yet, except for a thumbs up :)

Finally a small release issue I found while using this library
https://github.com/tweag/pthread/pull/4


Added a new q-s-m example, called Overflow, which fails only when there are >3 threads https://github.com/advancedtelematic/quickcheck-state-machine/pull/325

Discussed many issues with Stevan:

I discussed with Stevan issue (https://github.com/advancedtelematic/quickcheck-state-machine/issues/326), which is related to  how to handle exceptions and whether testing should continue when they arise. Lead to pr https://github.com/advancedtelematic/quickcheck-state-machine/pull/331.

With many small prs tried to fix existing tests, which often made CI fail (discussed at https://github.com/advancedtelematic/quickcheck-state-machine/issues/327). Lead to prs 
- https://github.com/advancedtelematic/quickcheck-state-machine/pull/328 (which fixes some random generators), 
- https://github.com/advancedtelematic/quickcheck-state-machine/pull/330 (some bug in tests),
- https://github.com/advancedtelematic/quickcheck-state-machine/pull/337 (an exception from graphviz). Related to the last pr, I also fixed a small bug in graphviz https://github.com/ivan-m/graphviz/pull/45 (not merged yet).

One pr (https://github.com/advancedtelematic/quickcheck-state-machine/pull/339) which fixes the labeling of q-s-m (which was a bit verbose) and replaces the default output.Related pr which also improved the output of tests https://github.com/advancedtelematic/quickcheck-state-machine/pull/341. Other related issues and work in progress: https://github.com/advancedtelematic/quickcheck-state-machine/issues/342, https://github.com/advancedtelematic/quickcheck-state-machine/issues/340

With Stevan we decided to test rqlite, a distributed database. To make this possible, I wrote a Haskell client for rqlite and published it on Hackage (http://hackage.haskell.org/package/hs-rqlite). This is the first package I publish. Using this client I wrote a pretty big test https://github.com/advancedtelematic/quickcheck-state-machine/tree/rqlite, which I believe will soon be merged at q-s-m. In this test I use the `process` package to dynamically spawn unix processes during tests, which run rqlite servers and also kill them and restart them. I am also currently working on a tool which can create network partitions https://github.com/worstcase/blockade and trying to integrate it with my tests. About these tests and the client, I am writing a new blog-post https://github.com/kderme/gsoc/blob/master/blog/rqlite.md.

I recently added
Finally I'm working on a issue related to doing proper clean-up after each q-s-m test finishes https://github.com/advancedtelematic/quickcheck-state-machine/issues/335. I believe this issue sometimes makes q-s-m hard to use. My solution is for now integrated with my rqlite test (maybe I'll do a different pr though).

