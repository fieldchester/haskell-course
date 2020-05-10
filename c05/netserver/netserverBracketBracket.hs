
module Main(main) where

import Network
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad(forever)


main = withSocketsDo $ do
    bracket (listenOn $ PortNumber 8000) (sClose) $ \sock ->
            putStrLn "listening on port" *>
            (forever $
                putStrLn "waiting for connection" *>
                (bracket (accept sock) (\(h,_,_) -> hClose h) $ \(h,_,_) -> -- hClose re-raises
                    putStrLn "client connected" *>
                    hGetChar h *>
                    threadDelay (2000*1000) *>
                    hPutStr h msg *>
                    hFlush h))

msg =  "HTTP/1.0 200 OK\r\nContent-Length: 7\r\n\r\nPong!\r\n"
