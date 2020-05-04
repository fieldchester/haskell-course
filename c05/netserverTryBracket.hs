{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import Network
import System.IO
import Control.Concurrent
import Prelude hiding (catch)
import Control.Exception
import Control.Monad(forever)

-- no async exception here, so please don't type Ctrl - C

data MyEx = AnEx Socket
instance Exception MyEx
instance Show MyEx where
   show (AnEx s) = show s

exPred :: SomeException -> Maybe Socket
exPred e
      | Just ((AnEx s) :: MyEx) <- fromException e = Just s
      | otherwise = Nothing

main = withSocketsDo $ do
    tryJust exPred pingPongAct >>= \r ->
        case r of
            Left listenSocket -> (putStrLn $ "catched sync MyEx, closing " ++ show s) *> sClose listenSocket
            Right req -> putStrLn $ "client : " ++ req -- to get here: comment-in forever and throw

-- or try (evaluate (5 `div` 0)) :: IO (Either SomeException Int) >>= \r

pingPongAct :: IO String
pingPongAct =
    (listenOn $ PortNumber 8000) >>= \sock ->
    putStrLn "listening on port" *>
    (forever $
        putStrLn "waiting for connection" *>
        (bracket (accept sock) (\(h,_,_) -> hClose h) $ \(h,_,_) -> -- hClose re-raise
            putStrLn "client connected" *>
            hGetChar h *>
            threadDelay (2000*1000) *>
            (throw $ toException $ AnEx sock) *>              -- raise, synchron
            hPutStr h msg *>
            hFlush h *>                                       -- re-raise
            pure "Ping!")  
    )

msg =  "HTTP/1.0 200 OK\r\nContent-Length: 7\r\n\r\nPong!\r\n"


