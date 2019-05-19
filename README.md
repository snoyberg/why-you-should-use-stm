# Why you should use Software Transactional Memory

This repo was originally prepared for a LambdaConf 2019 workshop by
Michael Snoyman. It contains code samples and exercises, as well as
content in this README.

## Setup

In order to participate with this workshop, please ensure that you do
the following _before the workshop begins_:

* Install the Stack build tool: https://haskell.fpcomplete.com/get-started
* Build the libraries we'll be using: `stack build --resolver lts-13.21 async stm-chans hspec`

## Prerequisites

This workshop will assume _no prior knowledge_ of Software
Transactional Memory (STM), the async library, or other related
concepts. Instead, you will be expected to understand:

* Basics of monads and `do`-notation
* The overall concept of mutable vs immutable variables
* A general understanding of race conditions and concurrency will be helpful

This articles may be helpful:
https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads.

## Format of this workshop

This workshop will be *interactive*. I'll be following this README and
asking for audience participation. This is not a two hour lecture,
that will be boring!

There will be exercises throughout. For each exercise, everyone will
have 5-10 minutes to work on it themselves, and then we'll solve it
together. Let's practice with exercise 00.

## Mutable variables

In Haskell, values are immutable. The following code does not make sense:

```haskell
myFunc = do
  let x = 5
  x += 1
  print x
```

Instead, you can define a _new_ variable that references the old one:

```haskell
myFunc = do
  let x = 5
  let y = x + 1
  print y
```

**QUESTION** Can anyone give me an example of a limitation of
immutable values?

Haskell still allows mutability, but it has to be *opt-in*. One simple
way of doing that is with `IORef`. Let's explore that with exercise 01.

**Proceed to exercise 01**

## What about STM?

An `IORef` works like a mutable variable in most other languages. This
makes it vulnerable to data races, and means the only real operations
we can perform on it are reading and writing. As we'll see, STM gives
us both data race protection, and more advanced behavior like retry.

There are two basic building blocks for `STM`:

* The `STM` type constructor. It provides a monadic interface, as well
  as an `Alternative` instance.
* The `TVar` type constructor, which is the `STM` equivalent of an
  `IORef`.

In place of `readIORef`, we have `readTVar`. Instead of `writeIORef`,
we have `writeTVar`. However, there's one important difference:
instead of living in `IO`, these operations live in `STM`:

```haskell
readIORef :: IORef a -> IO  a
readTVar  :: TVar a  -> STM a

writeIORef :: IORef a -> a -> IO  ()
writeTVar  :: TVar a  -> a -> STM ()

atomically :: STM a -> IO a

newIORef  :: a -> IO  (IORef a)
newTVar   :: a -> STM (TVar  a)
newTVarIO :: a -> IO  (TVar  a)
```

**QUESTION** Why do you think we need a `newTVarIO`? We'll get into
the full details later.

**QUESTION** Why do all of these operations need to live in `IO` or
`STM` at all? Why can't they be pure functions?

We're going to try converting our `IORef` code from exercise 01 over
to STM.

**Proceed to exercise 02**

## What does atomically mean?

The big advantage of STM is that, within a single call to
`atomically`, all changes to variables either commit at once, or not
at all. This helps avoid data races. In order to make this work, we
need to make sure that related actions are in the same transaction.

**Proceed to exercise 03**

In this exercise, we have multiple threads operating on the same 2
`TVar`s. To spell it out, the data race we're concerned about may look
something like:

1. Thread A reads balance of `from` as $100
2. Thread B reads balance of `from` as $100
3. Thread A reads balance of `to` as $50
4. Thread A writes balance of `from` to $95
5. Thread A writes balance of `to` to $55
6. Thread B reads balance of `to` as $55
7. Thread B writes balance of `from` to $95
8. Thread B writes balance of `to` as $60

In this scenario, we started with $100 in the `from` account and $50
in the `to` account, and ended with $95 and $60. We just created $5
out of thin air!

However, our solution avoids this possibility, by putting all four
interactions with these two variables per thread into its own
transaction.

In an STM transaction, each time a variable is read or written, a
notation is made in the transaction log, internally to GHC's
runtime. When it comes time to **commit** the transaction, the runtime
checks the transaction log against the current state of those
variables. If any of the variables were read or written, the
transaction is **retried**.

We'll look more deeply into this mechanism. But first, let's introduce
some more explicit retrying so it's easier to talk about.

## Retrying

You can explicitly decide to retry a transaction, such as if some
condition doesn't hold. Let's implement another one of my favorite
examples. We'll simulate Alice receiving a paycheck and, when she has
enough money, she'll transfer a lump sum payment to Bob.

**Proceed to exercise 04**

You can explicitly use the `retry` or `check` to trigger a retry of
the transaction. When this occurs, the GHC runtime notes which
variables have been viewed so far by the current transaction, and puts
the thread to sleep until one of those variables is modified. Since
the only side effects allowed in a transaction are those that interact
with `TVar`s, we know that retrying a transaction beforehand will be
useless.

**QUESTION** Can someone think of a way that the guarantees in the previous paragraph can be violated?

This means that we can use STM to easily implement blocking
algorithms, such as waiting for a value on a queue.

## TMVars

While the `TVar` is the basic data type in STM, there are other
structures built on top of `TVar`s. First, we'll talk about a `TMVar`.

`TMVar`s are a fusion of `MVar`s and `TVar`s. Essentially, they are a
`TVar` which can be emptied and filled. Our next exercise will
implement them.

**Proceed to exercise 05**

## Alternative

There was a small amount of duplication in that implementation around
the `try` version of take and put. That's a great opening to discuss
the `Alternative` instance of `STM`. The `Alternative` typeclass
introduces two methods: an operator called `<|>` and an identifier
`empty`. The meaning of `<|>` in the case of STM is:

> When running `a <|> b`, try to commit the transaction `a`. If `a`
> retries, then try to commit the transaction `b`. If both of them
> retry, then the combination of the two retries.

**QUESTION** What must be the meaning of `empty` in order for the
`Alternative` laws to be satisfied?

We can use the `Alternative` instance for `STM` to build up some very
sophisticated ideas from simple components. This is one of the things
that makes stm so powerful and composable. We'll take a simpler
example for our next exercise, based on what we just did in exercise
5.

**Proceed to exercise 06**

## Channels/queues/FIFOs

`TChan`s and `TQueue`s are similar structures: they provide a FIFO
queue of items. A `TChan` is slightly more powerful than a `TQueue`,
while the latter is faster. If you don't need the extra power
(broadcast channels), stick to `TQueue`s.

There are also quite a few variations of `TChan`s and `TQueue`s
between the [`stm`](https://www.stackage.org/package/stm) and
[`stm-chans`](https://www.stackage.org/package/stm-chans)
libraries. The variations are:

* Closeable? It has an `M` in it (since it acts a bit like an `MVar`)
* Bounded? It has a `B` in it
* `Chan` vs `Queue`

This ends up with 8 different FIFO structures:

1. `TChan`
2. `TBChan`
3. `TBMChan`
4. `TMChan`
5. `TQueue`
6. `TBQueue`
7. `TBMQueue`
8. `TMQueue`

In my opinion, the `TBMQueue` is the most generally useful of these. I
wish it was in the `stm` library itself, but you'll need to pull it in
from `stm-chans`. The combination of bounded, closeable queues and the
`async` library can open the door for some really beautiful concurrent
helper functions.

**Proceed to exercise 07**

**QUESTION** This exercise demonstrates why I like `TBMQueue`s so
much. What's good about the bounded and closeable aspects?

If you'd like to use these kinds of functions in your own code, you
can check out
[`UnliftIO.Async`](https://www.stackage.org/haddock/lts-13.21/unliftio-0.2.10/UnliftIO-Async.html#g:9).

## Deadlocks

STM helps make race conditions less likely. But as we've already seen,
it can't make them disappear. Similarly, it helps with deadlocks, but
can't get rid of them entirely. We'll skip the exercise on this one,
and instead just give an example of a deadlock.

**Proceed to fake exercise 08**

If you're lucky when running this, you'll see:

```
solution.hs: thread blocked indefinitely in an STM transaction
```

GHC is pretty good at detecting when a deadlock has occurred, which on
its own is pretty amazing. GHC can detect that all threads which have
access to the variables our current transaction is waiting for are
also waiting for this thread, and then kill all of them with an
asynchronous exception. This is nice, but has some downsides:

* Asynchronous exceptions can be confusing
* The detections doesn't work 100% reliably; it depends on garbage collection and can be fooled by some cases
* GHC will kill _all_ threads in the blocked state, even if in theory killing just one of the threads would unblock some of the others

In other words, this is a great convenience feature. But you shouldn't
write code that relies on the deadlock detection! Like any other
language, avoid deadlocks!

## Global variables

For our final exercise, we're going to play with global variables in
STM. We've already discussed the function you need to get this code to
work correctly. Run the exercise, see what fails, and then try to fix
it. We'll discuss why it fails afterwards.

**Proceed to fake exercise 09**

## Extra credit

Here are some additional concepts that we can discuss if people are
interested:

* Batons: using `TMVar`s (or `MVar`s) to notify another thread it can
  start working. Great use case: web server is ready to listen to
  connections. See [the Warp test
  suite](https://github.com/yesodweb/wai/blob/16af7026b7722f22340977f80ca8aa174c582ad0/warp/test/RunSpec.hs#L128-L145).
* You can write some nifty time-based code using `registerDelay`. An
  example that avoids `race` is [the complainer code in
  Stack](https://github.com/commercialhaskell/stack/commit/330152b78cbec38f0f34baa8135df8bc3fdb5bcb#commitcomment-33004596)
* You can't directly perform I/O inside `STM` (unless you cheat), but
  you _can_ interact with existing `Async`s.
