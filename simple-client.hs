{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Concurrent.Async
import qualified Control.Exception        as E
import           Control.Monad
import qualified Data.ByteString.Char8    as S
import           Network
import           Options.Applicative
import           System.IO

import           Util

opts :: ParserInfo ClientConfig
opts = info (helper <*> clientConfig)
  ( fullDesc
  & progDesc "echo client by Network"
  & header "simpleclient" )

main :: IO ()
main = withSocketsDo $ execParser opts >>= simpleclient

simpleclient :: ClientConfig -> IO ()
simpleclient ClientConfig {..} = benchQPS ccTotal $ do
  ws <- replicateM ccConn $ async $ do
    h <- connectTo ccHost (PortNumber $ fromIntegral ccPort)
    (`E.finally` hClose h) $ replicateM_ (ccTotal `div` ccConn) $ do
      S.hPut h "hello\n"
      hFlush h
      "hello\n" <- S.hGet h 6
      return ()
  mapM_ wait ws
