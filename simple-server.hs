{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Concurrent
import qualified Control.Exception   as E
import           Control.Monad
import qualified Data.ByteString     as S
import           Network
import           Options.Applicative
import           System.IO

import           Util

opts :: ParserInfo ServerConfig
opts = info (helper <*> serverConfig)
  ( fullDesc
  & progDesc "echo server by Conduit"
  & header "conduit-server" )

main :: IO ()
main = withSocketsDo $ execParser opts >>= simpleServer

simpleServer :: ServerConfig -> IO ()
simpleServer ServerConfig {..} = do
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
      E.handle (\_ioe -> (_ioe :: IOError) `seq` return ()) $ do
        go `E.finally` hClose h
