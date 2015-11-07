module XMLPipe where

import System.IO (Handle, hFlush, hPutStr)
import Data.IORef
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import DodgyXML

type XMLReceiver = IORef [Content]

xmlReceiver :: Handle -> IO XMLReceiver
xmlReceiver h = do
  contents <- L.hGetContents h
  let xml = parseXML contents
  newIORef xml
  
xmlReceive :: XMLReceiver -> IO (Maybe Content)
xmlReceive ref = do
  xml <- readIORef ref
  case xml of
    [] -> do
      return Nothing
    (x:xs) -> do
      writeIORef ref xs
      return (Just x)

type XMLSender = Handle

xmlSender :: Handle -> IO XMLSender
xmlSender h = return h

xmlSend :: XMLSender -> Content -> IO ()
xmlSend h xml = do
  let str = showContent xml
  T.putStr str
  T.hPutStr h str
  hFlush h
