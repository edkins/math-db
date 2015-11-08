{-# LANGUAGE OverloadedStrings #-}
module Coq where

import GHC.IO.Handle (Handle)
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import System.IO (hFlush)
import qualified Data.Text.Lazy.Encoding (encodeUtf8)

import AppIO
import DodgyXML
import XMLPipe

startCoq :: IO CoqProcess
startCoq = do
  (Just stdin, Just stdout, _, process) <- createProcess (proc "coqtop" ["-ideslave"]) {
    std_in = CreatePipe, std_out = CreatePipe}
  send <- xmlSender stdin
  rcv <- xmlReceiver stdout
  return CoqProcess {send = send, rcv = rcv, process = process}

stopCoq :: CoqProcess -> IO ()
stopCoq proc = do
  let call = Tag "call" [("val","quit")] []
  xmlSend (send proc) call
  ack <- xmlReceive (rcv proc)
  waitForProcess (process proc)
  return ()

coqInteract :: Content -> AppIO Content
coqInteract el = do
  sender <- coqSend
  receiver <- coqRcv
  send <- appIO (xmlSend sender el)
  appLog CoqSend send
  (rcv, result) <- appIO (xmlReceive receiver)
  appLog CoqReceive rcv
  return result

coqCall :: T.Text -> [Attrib] -> [Content] -> AppIO Content
coqCall val attrs contents = coqInteract $ Tag "call" (("val",val):attrs) contents

coqAbout :: AppIO Content
coqAbout = coqCall "about" [] []

coqInterp :: T.Text -> AppIO Content
coqInterp text = coqCall "interp" [("id","0"),("raw","")] [String text]

coqCheck :: T.Text -> AppIO Content
coqCheck text = coqInterp ("Check " `T.append` text `T.append` ".")

coqLookup :: T.Text -> AppIO T.Text
coqLookup text = do
  about <- coqAbout
  check <- coqCheck text
  return (showContent about `T.append` showContent check)

coqIndex :: AppIO T.Text
coqIndex = do
  about <- coqAbout
  return (showContent about)

coqChunks :: [T.Text] -> AppIO T.Text
coqChunks [] = coqIndex
coqChunks [text] = coqLookup text
coqChunks _ = error "Too many path elements"
