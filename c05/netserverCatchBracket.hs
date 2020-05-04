{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import Network
import System.IO
import Control.Concurrent
import Prelude hiding (catch)
import Control.Exception
import Control.Monad(forever)

-- data AsyncException UserInterrupt e.g. Control-C
--  or ThreadKilled or stack/heap overflow

data MyEx = AnEx Socket
instance Exception MyEx 
instance Show MyEx where
   show (AnEx s) = show s

handler :: SomeException -> IO ()
handler e 
        | Just (_ :: AsyncException) <- fromException e =
            putStrLn ("catched async: " ++ displayException e ++ ", throw to IO") *> throwIO e
        | Just ((AnEx listenSocket) :: MyEx) <- fromException e =
            putStrLn "catched sync MyEx, closing " *> sClose listenSocket
        | otherwise =
            putStrLn ("catched neither async nor sync MyEx: " ++ displayException e ++ ", re-throw to IO") *> throwIO e

main = withSocketsDo $ do
    catch pingPongAct handler

pingPongAct :: IO ()
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
            hFlush h))                                      -- re-raise

msg =  "HTTP/1.0 200 OK\r\nContent-Length: 7\r\n\r\nPong!\r\n"
