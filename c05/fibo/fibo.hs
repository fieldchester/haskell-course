module Main(main) where

import Control.Parallel

main :: IO ()
main =
  -- print (fib 34) -- Total   time    1.043s  (  1.044s elapsed)
  print (parfib 34) -- Total   time    2.096s  (  0.542s elapsed)

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


-- cabal v2-update
-- cabal get parallel-3.2.2.0
-- cd parallel-3.2.2.0; cabal v2-install --lib
-- cd ..

-- | Compile
-- ghc fibo.hs              for fib 34
-- ghc -threaded fibo.hs    for parfib 34
                      
-- | Run
-- Statistics (-s)
-- ./fibo +RTS -s           for fib 34
-- ./fibo +RTS -s -N        for parfib 34

-- | Run & Threadscope
-- Threadscope (-l)
-- ghc -eventlog ...
-- ./fibo +RTS -l -N        -l for enventlog
-- threadscope fibo.eventlog

-- | Compile
-- ghc -threaded -with-rtsopts="-s -l -N"
--                           no need to wite +RTS -s -l
-- ./fibo                    is all that is needed
