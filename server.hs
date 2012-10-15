{-# LANGUAGE OverloadedStrings #-}

import Network
import System.IO

main :: IO ()
main = do
  sock <- listenOn (PortNumber 1234)
  (h, _hostName, _portNumber) <- accept sock
  hGetContents h >>= hPutStr h
