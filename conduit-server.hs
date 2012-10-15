{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Network
import Network.Socket
import qualified Control.Exception as E

main :: IO ()
main = do
  let ss = (serverSettings 1234 "*")
        { serverAfterAccept = \sock -> setSocketOption sock NoDelay 1 }
  runTCPServer ss $ \ad -> do
    E.handle (\ioe -> (ioe :: IOError) `seq` return ()) $ do
      appSource ad $$ appSink ad
