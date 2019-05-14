#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Data.IORef

main :: IO ()
main = do
  -- Create a new reference
  ref <- newIORef "Hello"

  {- Original code
  origVal <- readIORef ref
  writeIORef ref (origVal ++ " World")
  -}

  -- Cheekier version
  modifyIORef ref (++ " World")

  -- This line won't compile, fix it!
  newVal <- readIORef ref
  putStrLn newVal
