{-# LANGUAGE OverloadedStrings #-}

import           Network
import           System.IO
import qualified Data.ByteString as S
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as E

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber 1234)
  forever $ do
    (h, _hostName, _portNumber) <- accept sock
    forkIO $ do
      let go = do
            bs <- S.hGetSome h 1024
            when (not $ S.null bs) $ do
              S.hPut h bs
              hFlush h
              go
      E.handle (\_ioe -> (_ioe :: IOError) `seq` return ()) go
      E.handle (\_ioe -> (_ioe :: IOError) `seq` return ()) $ hClose h
