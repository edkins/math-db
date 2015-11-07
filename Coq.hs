{-# LANGUAGE OverloadedStrings #-}
module Coq where

import GHC.IO.Handle (Handle)
import System.Process
import Data.Text as T
import Data.Text.IO as T
import Data.ByteString.Lazy as B
import System.IO (hFlush)
import Data.Text.Lazy.Encoding (encodeUtf8)

import DodgyXML
import XMLPipe

data CoqProcess =
  CoqProcess {
    send :: XMLSender,
    rcv :: XMLReceiver,
    process :: ProcessHandle
  }

startCoq :: IO CoqProcess
startCoq = do
  (Just stdin, Just stdout, _, process) <- createProcess (proc "coqtop" ["-ideslave"]) {
    std_in = CreatePipe, std_out = CreatePipe}
  send <- xmlSender stdin
  rcv <- xmlReceiver stdout
  return CoqProcess {send = send, rcv = rcv, process = process}

type CoqExpr = Text
type CoqType = CoqExpr
type CoqVersion = Text

asCoqExpr :: Text -> IO CoqExpr
asCoqExpr str = return str

stopCoq :: CoqProcess -> IO ()
stopCoq proc = do
  let call = Tag "call" [("val","quit")] []
  xmlSend (send proc) call
  Just ack <- xmlReceive (rcv proc)
  waitForProcess (process proc)
  return ()

coqGetVersion :: CoqProcess -> IO CoqVersion
coqGetVersion proc = do
  let call = Tag "call" [("val","about")] []
  xmlSend (send proc) call
  Just version <- xmlReceive (rcv proc)
  return $ showContent version

coqGetType :: CoqProcess -> CoqExpr -> IO (CoqExpr, CoqType)
coqGetType proc expr = do
  let str = String $ T.append "Check " (T.append expr ".")
  let call = Tag "call" [("val","interp"),("id","0"),("raw","")] [str]
  xmlSend (send proc) call
  Just typ <- xmlReceive (rcv proc)
  T.putStrLn (showContent typ)
  return ("foo",showContent typ)

