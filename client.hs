{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Char8 as S
import qualified Control.Exception as E
-- import Control.Concurrent
import Control.Concurrent.Async
import Data.Time
import System.Environment
import Control.Applicative

main :: IO ()
main = do
  [con, req] <- map read <$> getArgs

  addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "1234")
  let serveraddr = head addrinfos
  ass <- replicateM con $ async $ do
    sock <- socket AF_INET Stream 0
    connect sock (addrAddress serveraddr)
    setSocketOption sock NoDelay 1
    replicateM_ (req `div` con) $ do
      NSB.sendAll sock "hello\n"
      "hello\n" <- NSB.recv sock 6
      return ()
    return () `E.finally` close sock

  start <- getCurrentTime
  mapM_ wait ass
  end <- getCurrentTime

  print $ end `diffUTCTime` start
