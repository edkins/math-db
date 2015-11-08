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
import Expr

type CoqAbout = T.Text

data CoqReport = CoqReport {
  lookupExpr :: T.Text,
  expr :: Expr,
  typ :: Expr,
  about :: CoqAbout
} deriving Show

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

xmlToAbout = showContent

coqAbout :: AppIO CoqAbout
coqAbout = do
  xml <- coqCall "about" [] []
  return (xmlToAbout xml)

fetchInterpText :: Content -> AppIO T.Text
fetchInterpText value =
  let string = xmlMatchTag "value" [("val","good")] [value] in
  let text = xmlMatchTag "string" [] string in
  return (xmlMatchString text)

coqInterp :: T.Text -> AppIO T.Text
coqInterp text = do
  xml <- coqCall "interp" [("id","0"),("raw","")] [String text]
  fetchInterpText xml

coqCheck :: T.Text -> AppIO Expr
coqCheck text = do
  text <- coqInterp ("Check " `T.append` text `T.append` ".")
  return (coqParseTypeJudgement text)

coqLookup :: T.Text -> AppIO CoqReport
coqLookup text = do
  about <- coqAbout
  check <- coqCheck text
  let (expr,typ) = unpackTypeJudgement check
  return $ CoqReport {
    lookupExpr = text,
    expr = expr,
    typ = typ,
    about = about
  }

coqIndex :: AppIO T.Text
coqIndex = do
  about <- coqAbout
  return about

coqChunks :: [T.Text] -> AppIO T.Text
coqChunks [] = coqIndex
coqChunks [text] = do
  report <- coqLookup text
  return $ T.pack $ show report
coqChunks _ = error "Too many path elements"
