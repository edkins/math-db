{-# LANGUAGE OverloadedStrings #-}
module XMLPipe where

import System.IO (Handle, hFlush, hPutStr)
import Data.IORef
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Data.Attoparsec.Text
import DodgyXML

type XMLReceiver = Handle

xmlReceiver :: Handle -> IO XMLReceiver
xmlReceiver h = return h

xmlReceive :: XMLReceiver -> IO Content
xmlReceive h = do
  chunk <- T.hGetChunk h
  T.putStrLn chunk
  let result = parse parseItem chunk
  processResult h result

processResult :: Handle -> Result Content -> IO Content
processResult h (Fail _ _ msg) = error msg
processResult h (Done leftover r)
  | leftover == "" = return r
  | otherwise = fail ("Left over: " ++ T.unpack leftover)
processResult h (Partial c) = do
  chunk <- T.hGetChunk h
  let result = c chunk
  processResult h result

type XMLSender = Handle

xmlSender :: Handle -> IO XMLSender
xmlSender h = return h

xmlSend :: XMLSender -> Content -> IO ()
xmlSend h xml = do
  let str = showContent xml
  T.putStrLn str
  T.hPutStr h str
  hFlush h
