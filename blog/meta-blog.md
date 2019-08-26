In my gsoc-2019 project, I used [quickcheck-state-machine](https://github.com/advancedtelematic/quickcheck-state-machine) 
to write tests about different dbs, with the aim to test new fanctionality that I added to these dbs, provide good examples 
of how to write tests using q-s-m, and also improve q-s-m itself.

In this [blog](https://github.com/kderme/gsoc/blob/master/blog/References.md) I talk about some internal information of 
q-s-m and how I fixed some bugs, related to the References that q-s-m uses. 
[Here](https://github.com/kderme/gsoc/blob/master/blog/cleanup.md) I wrote about adding a cleanup new function in q-s-m, which
I believe can make writing q-s-m tests much easier.

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

My [final report](https://github.com/kderme/gsoc/blob/master/blog/report.md) has a comprehensive lists of all my prs during 
gsoc-2019
