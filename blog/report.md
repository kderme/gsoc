I wrote many q-s-m tests for persistent
https://github.com/kderme/gsoc
I used this test suite to find bugs in q-s-m itself, but I also found a small bug in persistent and I prepared a pr which fixes it
https://github.com/yesodweb/persistent/pull/915 (hasn't received any review yet) and https://github.com/yesodweb/persistent/pull/922

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
