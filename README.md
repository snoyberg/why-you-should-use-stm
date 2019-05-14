# Why you should use Software Transactional Memory

This repo was originally prepared for a LambdaConf 2019 workshop by
Michael Snoyman. It contains code samples and exercises, as well as
content in this README.

## Setup

In order to participate with this workshop, please ensure that you do
the following _before the workshop begins_:

* Install the Stack build tool: https://haskell.fpcomplete.com/get-started
* Build the libraries we'll be using: `stack build --resolver lts-13.21 rio stm-chans hspec`

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

## FIXME

* https://github.com/commercialhaskell/stack/commit/330152b78cbec38f0f34baa8135df8bc3fdb5bcb#commitcomment-33004596
* https://www.stackage.org/haddock/lts-13.19/unliftio-0.2.10/src/UnliftIO.Internals.Async.html#pooledMapConcurrentlyIO%27
* `newTVarIO` and global variables
* The Warp test case: use check to block instead of spinlocking. 
