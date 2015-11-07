module XMLPipe where

import System.IO (Handle, hFlush, hPutStr)
import Data.IORef
import Data.ByteString.Lazy as B

import DodgyXML

type XMLReceiver = IORef [Content]

xmlReceiver :: Handle -> IO XMLReceiver
xmlReceiver h = do
  contents <- B.hGetContents h
  let xml = parseXML contents
  newIORef xml
  
xmlReceive :: XMLReceiver -> IO (Maybe Content)
xmlReceive ref = do
  Prelude.putStrLn "a"
  xml <- readIORef ref
  Prelude.putStrLn "b"
  case xml of
    [] -> do
      Prelude.putStrLn "c"
      return Nothing
    (x:xs) -> do
      Prelude.putStrLn "d"
      Prelude.putStrLn (show x)
      writeIORef ref xs
      return (Just x)

type XMLSender = Handle

xmlSender :: Handle -> IO XMLSender
xmlSender h = return h

xmlSend :: XMLSender -> Content -> IO ()
xmlSend h xml = do
  let str = showContent xml ++ "\n"
  Prelude.putStrLn str
  System.IO.hPutStr h str
  hFlush h
