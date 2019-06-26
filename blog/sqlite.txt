

SQlite is a in-process SQL database written in C. SQlite differs from other databases in that it uses a simple file for all storage and it is serverless. This means that if you want to use SQlite through Haskell you need to directly call into c code, using an interface called foreign function interface (FFI).
All Haskell libraries which provide these c bindings (like direct-sqlite, persistent..)  come with a c file, called The SQLite Amalgamation, which is 
a concatenation of all SQlite c code into a huge 220.000 lines file. The interraction with foreign functions is notorious for its complications. In this
blog we will discuss about proper ways to use SQlite in multithreaded applications, propose a new backend to use it, which we have implemented and tested through quickcheck-state-machine (more on this on another blog).

First let's get a bit familiar with SQlite. The SQLite project provides a simple, easy to install, command-line program named sqlite3.

Let's open an sqlite3 db by

# sqlite3 sqlite3.db

and let's create a tabe to have something to work on

> create table Person (name Text PRIMARY KEY, age Integer);
> insert into person (kostas, 25);

Databases use transaction to group together operations that we want to be executed atomically. In SQlite this is done
by wrapping them in begin; .. end; commands.

> begin;
> insert into person (kostas, 25);
> insert into person (John, 20);
> end;

This will fail because kostas is already here and name is a Primary Key. John will not be inserted either. 
Now let's try to open a transaction within a transaction

> begin;
> begin;
Error: cannot start a transaction within a transaction
> end;

ok we can't. By default SQlite is always in auto-commit mode. This means that for each command, SQLite starts, processes, and commits the transaction automatically.
A begin command makes an sqlite connection leave from autocommit mode. This is important because we don't want to commit each command separately, but group commands together. However trying to leave autocommit mode 2 times is a mistake. It may seem strange to even try to do this, but we later see that this is an important issue when two threads share the same db connection (this is something we can't test from sqlite3).

But what about a second connection? How does it interract with the existing connection?
Lets use sqlite3 again to open a second connection

			# sqlite3 sqlite3.db
			> .schema
			CREATE TABLE Person (name Text PRIMARY KEY, age Integer);

> begin;
			> begin;

No error...

> insert into Person(name,age) values("john", 42);
			> insert into Person(name,age) values("mark", 17);
			Error: database is locked

This means that calling insert is what actually uses the internal SQlite locks and not begin. Since insert is inside a transaction it will continue to hold the lock. When the second transaction tries to insert it finds the db locked.

> end;
			> insert into Person(name,age) values("mark", 17);
			> end;

There is a strict version of begin, which takes the locks immediatel:

> begin imediate;
			> begin imediate;
			Error: database is locked

as expected, this throws an exception even before trying to insert.

While the db is still busy, let's try something else:
# lsof sqlite3.db
COMMAND   PID   USER   FD   TYPE DEVICE SIZE/OFF    NODE NAME
sqlite3 24657 kostas    3ur  REG    8,7     1024 4725038 sqlite3.db
sqlite3 26082 kostas    3ur  REG    8,7     1024 4725038 sqlite3.db
# kill -9 24657

> Killed
			> begin imediate;

The db is unlocked for other connections.

Now let's try to get the same behaviour from persistent. For an introduction into persistent
this https://www.yesodweb.com/book/persistent is great. In this example we create our db, and let 2 threads write
to the same db connection concurrently.

main :: IO ()
main = do
    conn <- open "sqlite.db"
    (backend :: SqlBackend) <- wrapConnection conn (\_ _ _ _ -> return ())
    flip runSqlPersistM backend $ do
        runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
    p0 <- async $ thread0 backend
    p1 <- async $ thread1 backend
    _ <- waitBoth p0 p1
    close' backend
    return ()


thread0 :: SqlBackend -> IO ()
thread0'' backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlConn backend $ do
        insert_ $ Person ("kostas-" ++ show n) n

thread1 :: SqlBackend -> IO ()
thread1'' backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlConn backend $ do
        insert_ $ Person ("john-" ++ show n) n

When we run it we get this error message:
gsoc: SQLite3 returned ErrorError while attempting to perform step: cannot start a transaction within a transaction

Exactly what we got when we tried two `begins` from the same connection from sqlite3. Same conclusion: we can't run transaction using the
same connection. Let's try now to read:


thread0 :: SqlBackend -> IO ()
thread0 backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlConn backend $ do
        (pers :: [Entity Person]) <- selectList [] []
        return ()

thread1 :: SqlBackend -> IO ()
thread1 backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlConn backend $ do
        (pers :: [Entity Person]) <- selectList [] []
        return ()

gsoc: SQLite3 returned ErrorError while attempting to perform step: cannot start a transaction within a transaction.
Actually Persist wraps in transactions all sql queries, even if they simply read from the database.

It seems this is the wrong approach to use SQlite from multiple thread. We find that persistent provides a way to handle
multiple db connections. 

main :: IO ()
main = do
    poolBackend :: Pool SqlBackend <- runStdoutLoggingT $ createSqlitePool "dbs-test/sqlite.db" 2
    runStdoutLoggingT $ flip runSqlPool poolBackend $ do
	runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
    p0 <- async $ thread 0 poolBackend
    p1 <- async $ thread 1 poolBackend
    _ <- waitBoth p0 p1
    destroyAllResources poolBackend
    return ()

thread :: Int -> Pool SqlBackend -> IO ()
thread p backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlPool backend $ do
        insert_ $ Person (show p ++ "-" ++ show n) n


When we run this we soon get
gsoc: SQLite3 returned ErrorBusy while attempting to perform step: database is locked
which is the same error we noticed at sqlite3.

In order to fix this error, persistent provides a retryOnBusy combinator, which wraps a database action
and retries it when a ErrorBusy is encountered.


thread :: Int -> Pool SqlBackend -> IO ()
thread p backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlPool backend $ retryOnBusy $ do
        insert_ $ Person (show p ++ "-" ++ show n) n

This fixes the error. However if we try to look carefully how these querries are executed, we wil see that there is
no fairness between the two threads. This happens because in case of BusyErrors, the thread waits on an
exponential backoff. So if there is one thread which continuously writes, the other threads starve.

This is one of the issues we tried to solve. Our implementation puts all the request on a queue and there is a
single long living thread, which takes the request from th queue and run it. The thread is forked at the begining of the execution
and it waits for db actions on a forever loop. The threads which want to write to the db write their request on a queue, together
with an TMVar. Then it waits on the TMVar the result of the execution. In this way the user has no way to distinguish 

This simultaneously solves a different issue. Calling into foreign code has a big disadvantage: These threads cannot be interrupted.
This can be frustrating to the user, especially when he tries to stop the execution with a ^C action. This is the reason why it is usually advised
not to directly call into foreign code, but to fork a thread and let this thread do the foreign calls. 

Parallel & Concurrent Haskell:
do
 a <- async $ c_read fd buf size
 r <- wait a

This way the user thread waits on a TMVar for the execution to end. This makes the thread interruptible: a thread waiting on a STM can be easily
interrupted by the Haskell runtime.

As a matter of fact, this is what we do here. But instead of forking a thread each time we want to access the db, we fork a thread at the begining
which waits for db actions. Actually, when someone uses System.IO functions he uses a similar
functionality, provided by ghc, called the IO Manager. The I/O manager's job is to to provide a blocking I/O API to the user without forcing the RTS to create one operating system thread per Haskell thread. 

The RTS keeps a global list of pending events, unsuprising called pendingEvents, containing a elements of the following data type:
data IOReq
  = Read   Fd (MVar ())
  | Write  Fd (MVar ())

When a thread wants to read from a file descriptor fd it calls threadWaitRead which in turn calls waitForReadEvent.
waitForReadEvent :: Fd -> IO ()
waitForReadEvent fd = do
  m <- newEmptyMVar
  atomicModifyIORef pendingEvents (\xs -> (Read fd m : xs, ()))
  prodServiceThread
  takeMVar m

I/O manager provides other functionality which we don't and our implementation is not meant to replace it, by any means.
However our implementaion is surprisingly similar to this:

We have an action
data AsyncAction r = forall a. AsyncAction (r -> IO a) (TMVar (Either SomeException a))

r is the resource (in our case the db connection) that only the write thread has access to.
The TMVar has the response of the thread. This can be successful or some exception which is wrapped
inside SomeException.

and we can write on a queue (we used TBQueue in our implementation):
resp <- newEmptyTMVarIO
writeTBQueue asyncQueue $ AsyncAction action resp
ret <- atomically $ takeTMVar resp

Where actions is the db action the user thread wants to execute.
We used TMVar instead of MVars, in order to use the existing STM mechanism that TBQueue uses. This provides better
composability, but we may need to take a look at the performance implications (MVars are usually faster).

Our implementation is very similar to async and it was influenced by it a lot. The difference is that in async the thread lives
only for the duration of the execution of the IO action provided and cannot return the result without dying. In our case we want
a thread which continues to live. Another package called immortal provides this functionality, but it is more general purpose and
doesn't directly facilitate us.

There is an additional reason someone may want to have a long living thread. Some foreign libraries require to always be called by the same
os thread. OpenGL is usually mentioned as one of these cases. These libraries keep an implicit state, based on the os thread which called
them. Calling them by different os thread means that this implicit state gets lost. The reason we keep mentioning os thread here is because in
general Haskell threads do not directly map to os threads. The Haskell runtime manages a limited pool of OS worker threads (if -threaded is not specified they will be a single thread) and multiplexes Haskell thread on them. This approach allows extremely lightweight threads, since context-switching requires no OS interraction. However this also means that Haskell threads can jump around between different OS threads. Haskel provides a mechanism to keep a thread bound to an os thread: forkOS and asyncBound use this functionality. In our implementation we extend further this functionality: we give the option to make the long living thread os bound. By doing so all foreign calls are always executed by the same thread, without any effort from the programmer. We haven't explored using async-queue in these kind of libraries.

We are not entirely convinced that using OS bound threads is necessary in SQlite, but investigating this issue brought us to a shocking realization. By default SQlite is used in Serialized mode, which means that it can be "safely" used by two threads. As we saw above though, two threads cannot open a transaction
We have noticed that SQlite uses some mutexes called recursive mutexes, which protect . These mutexes keep the notion of ownership: which thread owns them. This allows the same thread to take the same mutex again and again without deadlocking, with the responsibility of the same thread to unlock it the same number of times. SQite uses these mutexes and also exports them and suggests using them in some cases. We were tempted to try this out. What could possibly even go wrong after all... (This chapter may appear a bit out of focus but we found it very interesting to test. One could go to the next chapter where we present the results).

foreign import ccall "sqlite3_mutex_enter_connection"
  mutex_enterC :: Ptr () -> IO ()
mutex_enter :: Connection -> IO ()
mutex_enter (Connection _ (Connection' database)) = do
    mutex_enterC database


foreign import ccall "sqlite3_mutex_leave_connection"
  mutex_leaveC :: Ptr () -> IO ()
mutex_leave :: Connection -> IO ()
mutex_leave (Connection _ (Connection' database)) = 
    mutex_leaveC database

This creates bindings for the SQite mutexes of a connection.

main :: IO ()
main = do
    db <- open "sqlite3-mutex.db"
    mvar <- newMVar False
    a <- async $ mutexThread 0 db mvar
    b <- async $ mutexThread 1 db mvar
    _ <- replicateM 10 $ async $ forM_ [1..] $ \n -> 
		-- here we simulate the existence of other threads.
		-- These create bigger contention about the same
		-- os threads.
		dummyIOAction
    _ <- waitBoth a b
    return ()

mutexThread :: Int -> Connection -> MVar Bool -> IO ()
mutexThread n db mvar = forM_ [1..1000] $ \n -> do
    _ <- mutex_enter db
    -- critical area.
    modifyMVar_ mvar $ \x -> case x of
        True -> throwIO $ SyncError $ "Thread " ++ show n ++ " expected False on trial " ++ show n
        False -> return True
    dummyIOAction
    modifyMVar_ mvar $ \x -> case x of
        False -> throwIO $ SyncError $ "Thread " ++ show n ++ " expected True on trial " ++ show n
        True -> return False
    -- end of critical area.
    mutex_leave db
    print n

data SyncError = SyncError String deriving (Show, Typeable, Exception)
dummyIOAction :: IO ()
dummyIOAction = writeFile "/dev/null" (show $ fib 35)

We have 2 threads trying to run on a critical and we protect this critical area with these mutexes. Note that we run some additional which run dummy actions. This creates bigger contention to the scheduler for the os threads available. In addition we compile with -threaded -rtsopts "-with-rtsopts=-N2", which allows using only 2 os threads. This makes contention even bigger. Note that in this implementation we don't take good care of exceptions and brackets.

If we try to execute this it will sometimes deadlock and sometimes fail, with something like
SyncError "Thread 6 expected False on trial 6"
On the other hand, if we replace async with asyncBound, the execution nothing bad happens.

I belieave it's pretty clear what happens at this point. If our Haskell thread gets context switched and later scheduled on a different os thread, while owning this mutex, the results are disastrous. We printed the thread ids at the begining of the critical area and after returning form the dummyIOAction and we confirmed that the threadId changes. Note that we are not referring to the thread Id returned by `Control.Concurrent.myThreadId` but from `System.Posix.Thread.myThreadId`, which is basically a system calls for unix like systems and returns the id of the os thread currently running. This thread id can dynamically change in the lifetime of a haskell thread which is not os bound.

What's interesting is that the program deadlocks even if only a single thread tries to enter the critical area. This is because as we mentioned the recursive mutexes remember the os thread which own them and can't allow other threads to unlock them.

If we compile our executable without -threaded, the Haskell runtime can only manage a single os thread and multiplexes all Haskell thread to it. This means that the mutex here provides no safety since all Haskell thread map to a single os thread and ownership of the mutex means that all Haskell thread can ownn it. The execution fails with:
SyncError "Thread 1 expected False on trial 1"
Deadlock cannot happen in this case. If we have a single thread trying to get in the critical area no error can occur. Also using asyncBound is impossible, since without -threaded, ghc has no way to know how to fork new os threads, so it fails:

user error (RTS doesn't support multiple OS threads (use ghc -threaded when linking))


All these test may appear somewhat abussive:
All these: using the SQlite mutexes outside of  may appear like abuses and to some point


NOTES
closing a write connection flushes all writes to disk, since it forces a 

fo ffis from forked threads 
	https://gitlab.haskell.org/ghc/ghc/issues/3937
	https://gitlab.haskell.org/ghc/ghc/issues/7353

IO manager does not support libraries usage:
https://stackoverflow.com/questions/4446700/what-io-activity-does-the-ghc-io-manager-support



When a Haskell thread is making a foreign call, it cannot receive asynchronous excep‐
tions. There is no way in general to interrupt a foreign call, so the runtime system waits
until the call returns before raising the exception. This means that a thread blocked in
a foreign call may be unresponsive to timeouts and interrupts, and moreover that calling
throwTo will block if the target thread is in a foreign call.
The trick for working around this limitation is to perform the foreign call in a separate
thread. For example:
Parallel & Concurrent Haskell
do
 a <- async $ c_read fd buf size
 r <- wait a

That's not something new. We actually copy what the I/O manager does:
https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/io-manager

