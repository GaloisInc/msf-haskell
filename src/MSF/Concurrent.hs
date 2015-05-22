-- |Higher-level concurrency in support of functions in "MSF.Event".
module MSF.Concurrent where

import MSF.Monad

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async,async,waitCatchSTM)
import Control.Concurrent.STM (STM,orElse,retry,atomically)
import Control.Exception (SomeException)
import Control.Monad (void)
import MonadLib (ask,inBase)


mapWithContext :: ([a] -> a -> [a] -> b) -> [a] -> [b]
mapWithContext step = go []
  where
  go ls as = case as of
    a:rs -> step ls a rs : go (a:ls) rs
    []   -> []

waitOneSTM :: [Async a] -> STM (Either SomeException a, [Async a])
waitOneSTM  = foldr orElse retry . mapWithContext step
  where
  step ls m rs = do
    e <- waitCatchSTM m
    return (e, ls ++ rs)


type ChildList a = [Async a]

-- | Wait until all children have finished.
waitForChildren :: ChildList b -> MSF a [Either SomeException b]
waitForChildren  = io . go
  where
  go cs
    | null cs   = return []
    | otherwise = do
        (e,rest) <- atomically (waitOneSTM cs)
        es       <- go rest
        return (e:es)


-- |Waits for a thread to finish and immediately runs the given
-- function on all of the values in the clReturns stack in
-- ChildList. This depleates the clReturns stack. Use instead of
-- waitForChildren if you want to process the return values as they
-- come in.
waitAndProcess :: ChildList b -> (Either SomeException b -> MSF a ())
               -> MSF a ()
waitAndProcess childList action = go childList
  where
  go cs
    | null cs   = return ()
    | otherwise = do
        (e,rest) <- io (atomically (waitOneSTM cs))
        action e
        go rest

-- |Run this list of MSF actions concurrently. Returns immediately
-- after forking with the handle for the threads.
runConcurrently :: [MSF a b] -> MSF a (ChildList b)
runConcurrently actions = MSF $ do
  env <- ask
  let fork action = async (runMSF action env)
  inBase (mapM fork actions)

-- | Simple fork / async.
spawn :: MSF s () -> MSF s ()
spawn spawned = MSF $ do
  env <- ask
  inBase (void (forkIO (runMSF spawned env)))
