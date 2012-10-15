{-# LANGUAGE OverloadedStrings, RecordWildCards        #-}

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8     ()
import           Data.IORef
import qualified GHC.Event                 as Ev
import           Network.Socket
import qualified Network.Socket.ByteString as NSB
import           Options.Applicative
import           System.Posix.Types

import           Util

main :: IO ()
main = execParser opts >>= evclient

opts :: ParserInfo ClientConfig
opts = info (helper <*> clientConfig)
  ( fullDesc
  & progDesc "echo client by GHC.Event"
  & header "evclient" )

evclient :: ClientConfig -> IO ()
evclient ClientConfig {..} = benchQPS ccTotal $ do
  addrinfos <- getAddrInfo Nothing (Just ccHost) (Just $ show ccPort)
  let serveraddr = head addrinfos

  em <- Ev.new

  socks <- replicateM ccConn $ do
    sock <- socket AF_INET Stream 0
    connect sock (addrAddress serveraddr)
    setSocketOption sock NoDelay 1
    return sock

  ior <- newIORef 0

  forM_ socks $ \sock -> do
    let cb _fd _key = do
          "hello\n" <- NSB.recv sock 6
          modifyIORef ior (+1)
          cur <- readIORef ior
          if cur < ccTotal
            then NSB.sendAll sock "hello\n"
            else Ev.shutdown em
    _ <- Ev.registerFd em cb (Fd $ fdSocket sock) Ev.evtRead
    return ()

  forM_ (take ccActive $ cycle socks) $ \sock -> do
    NSB.sendAll sock "hello\n"

  Ev.loop em
