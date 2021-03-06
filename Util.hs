module Util where

import           Data.Time
import           Options.Applicative
import           Text.Printf

data ClientConfig
  = ClientConfig
    { ccHost   :: String
    , ccPort   :: Int
    , ccConn   :: Int
    , ccActive :: Int
    , ccTotal  :: Int
    }

clientConfig :: Parser ClientConfig
clientConfig = ClientConfig
  <$> strOption
      ( long "host" & short 'h'
      & metavar "HOSTNAME" & value "localhost"
      & help "hostname" )
  <*> option
      ( long "port" & short 'p'
      & metavar "PORT" & value 1234
      & help "Port number" )
  <*> option
      ( long "connection" & short 'c'
      & metavar "CONN" & value 1
      & help "Connection number" )
  <*> option
      ( long "active" & short 'a'
      & metavar "ACTIVE" & value 1
      & help "Active request number" )
  <*> option
      ( long "requests" & short 'n'
      & metavar "REQUESTS" & value 10000
      & help "Request number" )

data ServerConfig
  = ServerConfig
    { scPort :: Int
    }

serverConfig :: Parser ServerConfig
serverConfig = ServerConfig
  <$> option
      ( long "port" & short 'p'
      & metavar "PORT" & value 1234
      & help "Port number" )

benchQPS :: Int -> IO () -> IO ()
benchQPS qnum m = do
  ela <- measureTime m
  printf "%d reqs in %.03f sec, %.03f qps\n" qnum ela (fromIntegral qnum / ela)

measureTime :: IO () -> IO Double
measureTime m = do
  start <- getCurrentTime
  _ <- m
  end <- getCurrentTime
  return . realToFrac $ end `diffUTCTime` start
