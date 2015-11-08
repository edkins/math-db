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

xmlReceive :: XMLReceiver -> IO (T.Text,Content)
xmlReceive h = do
  chunk <- T.hGetChunk h
--  T.putStrLn chunk
  let result = parse parseItem chunk
  processResult chunk h result

processResult :: T.Text -> Handle -> Result Content -> IO (T.Text,Content)
processResult t h (Fail _ _ msg) = error msg
processResult t h (Done leftover r)
  | leftover == "" = return (t,r)
  | otherwise = fail ("Left over: " ++ T.unpack leftover)
processResult t h (Partial c) = do
  chunk <- T.hGetChunk h
  let result = c chunk
  processResult (t `T.append` chunk) h result

type XMLSender = Handle

xmlSender :: Handle -> IO XMLSender
xmlSender h = return h

xmlSend :: XMLSender -> Content -> IO T.Text
xmlSend h xml = do
  let str = showContent xml
--  T.putStrLn str
  T.hPutStr h str
  hFlush h
  return str
