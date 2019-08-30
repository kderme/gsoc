# GSoC-2019

In my gsoc-2019 project, I used [quickcheck-state-machine](https://github.com/advancedtelematic/quickcheck-state-machine) 
to write tests about different dbs, with the aim to test new functionality that I added to these dbs, provide good examples 
of how to write tests using q-s-m, and also improve q-s-m itself. I also created a q-s-m test of a distributed database, which also allows injected errors, like partitioning of nodes, stopping and restoring nodes, pausing and unpausing.

In this [blog](https://github.com/kderme/gsoc/blob/master/blog/References.md) I talk about some internal information of 
q-s-m and how I fixed some bugs, related to the References that q-s-m uses. 
[Here](https://github.com/kderme/gsoc/blob/master/blog/cleanup.md) I wrote about adding a cleanup new function in q-s-m, which can make writing q-s-m tests much easier.

In this [blog](https://github.com/kderme/gsoc/blob/master/blog/blog-sqlite.md), I wrote about the most interesting stuff I
noticed while testing SQLite. Appart from simply writing tests, I developped a new way to access the SQLite in parallel, 
inspired by [this](https://news.ycombinator.com/item?id=20047918). In short, the idea is to fork a thread and pass all write
requests through a TBQueue, allowing only the forked thread to have access to the db for write operations.

After that I tested a disributed database, called [rqlite](https://github.com/rqlite/rqlite). 
Some issues I faced while testing this db are discussed
[here](https://github.com/kderme/gsoc/blob/master/blog/rqlite.md). In the same blog, I discuss about how I added fault
injection to my tests.

In my final [blog](https://github.com/kderme/gsoc/blob/master/blog/rqlite-test.md) I wrote about my tests of rqlite and
the consistency levels of rqlite and how I tried to hunt down some bugs.

In a number of prs I improved q-s-m trying to achive something similar to what [Jepsen](https://jepsen.io/) does: provide a framework which can be used to test complicated distributed systems. For example in [this](https://github.com/advancedtelematic/quickcheck-state-machine/pull/324) pr I generalized parallelism, so that more that 2 threads can be used. This allows to have a number of threads which inject errors, while other threads use the system api (Jepsen does something similar, with a thread called nemesis, which is used to inject errors). In another [pr](https://github.com/advancedtelematic/quickcheck-state-machine/pull/331) I improved the way q-s-m handles exceptions when using the system api.

My [final report](https://github.com/kderme/gsoc/blob/master/blog/report.md) has a comprehensive lists of all my prs during 
gsoc-2019

