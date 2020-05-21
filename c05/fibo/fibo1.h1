-- These list is a reminder
-- {-# OPTIONS -fglasgow-exts #-} deprecated
{-# OPTIONS_GHC -no-keep-hi-file #-}
{-# OPTIONS_GHC -no-keep-o-file #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}


module Main(main) where

import Control.Parallel
import Data.Time    -- diffent from tutorial

-- | Taken from
-- | A Tutorial on Parallel and Concurrent Programming in Haskell

main :: IO ()
main =
  getCurrentTime >>= \t0 ->
  seq (fib 34) (return ()) *>
  getCurrentTime >>= \t1 ->
  pseq (parfib 34) (return ()) *>
  getCurrentTime >>= \t2 ->
  print (diffUTCTime t1 t0) *>
  print (diffUTCTime t2 t1)


fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = f1 + f2
  where
    f1 = fib (n-1)
    f2 = fib (n-2)

parfib :: Int -> Integer
parfib n
  | n < 11 = fib n
  | otherwise = f1 `par` (f2 `pseq` (f1+f2))
  where
    f1 = parfib (n-1)
    f2 = parfib (n-2)


-- | ghc -threaded -eventlog -with-rtsopts="-s -l -N" fibo1.hs
-- | ./fibo1


-- | {-# OPTIONS_GHC -no-keep-hi-file #-}  and
-- | {-# OPTIONS_GHC -no-keep-o-file #-} need sth else to work
