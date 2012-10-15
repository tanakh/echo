{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString.Char8    as S
import           Data.Conduit
import           Data.Conduit.Network
import           Network                  (withSocketsDo)
import           Options.Applicative

import           Util

opts :: ParserInfo ClientConfig
opts = info (helper <*> clientConfig)
  ( fullDesc
  & progDesc "echo client by Conduit"
  & header "conduit-client" )

main :: IO ()
main = withSocketsDo $ execParser opts >>= conduitClient

conduitClient :: ClientConfig -> IO ()
conduitClient ClientConfig {..} = benchQPS ccTotal $ do
  ws <- replicateM ccConn $ async $ do
    runTCPClient (clientSettings ccPort $ S.pack ccHost) $ \ad -> do
      appSource ad $$ cond =$ appSink ad
  mapM_ wait ws
  where
    cond = replicateM_ (ccTotal `div` ccConn) $ do
      yield "hello\n"
      Just "hello\n" <- await
      return ()
