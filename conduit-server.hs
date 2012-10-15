{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Control.Exception    as E
import           Data.Conduit
import           Data.Conduit.Network
import           Network              (withSocketsDo)
import           Options.Applicative

import           Util

opts :: ParserInfo ServerConfig
opts = info (helper <*> serverConfig)
  ( fullDesc
  & progDesc "echo server by Conduit"
  & header "conduit-server" )

main :: IO ()
main = withSocketsDo $ execParser opts >>= conduitServer

conduitServer :: ServerConfig -> IO ()
conduitServer ServerConfig {..} = do
  runTCPServer (serverSettings scPort "*") $ \ad -> do
    E.handle (\ioe -> (ioe :: IOError) `seq` return ()) $ do
      appSource ad $$ appSink ad
