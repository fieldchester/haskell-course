module Main(main) where

import Network
import System.IO
import Control.Concurrent

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 8000
    loop sock
  where
    loop sock = do
        (h,_,_) <- accept sock
        body h
        loop sock

    body h = do
        _ <- hGetChar h
        threadDelay (5000*1000)
        hPutStr h msg >> hFlush h >> hClose h

    msg =  "HTTP/1.0 200 OK\r\nContent-Length: 7\r\n\r\nPong!\r\n"

-- curl http://localhost:8000
