{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Control.Exception         as E
import qualified Data.ByteString.Char8     as S
import qualified GHC.Event                 as Ev
import           Network.Socket
import qualified Network.Socket.ByteString as NSB
import           Options.Applicative
import           System.Posix.Types

import           Util

opts :: ParserInfo ServerConfig
opts = info (helper <*> serverConfig)
  ( fullDesc
  & progDesc "echo server by GHC.Event"
  & header "ev-server" )

main :: IO ()
main = withSocketsDo $ execParser opts >>= simpleServer

simpleServer :: ServerConfig -> IO ()
simpleServer ServerConfig {..} = do
  em <- Ev.new

  lsock <- socket AF_INET Stream defaultProtocol
  bind lsock $ SockAddrInet (fromIntegral scPort) iNADDR_ANY

  let acceptCB _fd _key = do
        (sock, _sockAddr) <- accept lsock
        Ev.registerFd em (echoCB sock) (Fd $ fdSocket sock) Ev.evtRead
        return ()

      echoCB sock fd _key = do
        bs <- E.handle (\_ior -> (_ior :: IOError) `seq` return "") $ do
          NSB.recv sock 1024
        if not $ S.null bs
          then do
          NSB.sendAll sock bs
          else do
          Ev.unregisterFd em fd
          close sock
          return ()

  Ev.registerFd em acceptCB (Fd $ fdSocket lsock) Ev.evtRead
  listen lsock maxListenQueue
  Ev.loop em
