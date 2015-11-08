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

data CoqAbout = CoqAbout {
  version :: T.Text,
  v1 :: T.Text,
  v2 :: T.Text,
  v3 :: T.Text
} deriving Show

data CoqReport = CoqReport {
  lookupExpr :: T.Text,
  expr :: Expr,
  typ :: Expr,
  about :: CoqAbout
} | CoqIndex { about :: CoqAbout } deriving Show

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

processFailure :: Content -> AppIO a
processFailure (Tag name attrs contents) =
  if name /= "value" then
    fail "Unexpected error when parsing return value"
  else if length attrs /= 3 ||
    attrs !! 0 /= ("val","fail") ||
    fst (attrs !! 1) /= "loc_s" ||
    fst (attrs !! 2) /= "loc_e" then
    fail "Unexpected attrs in error message"
  else do
    let loc_s = snd (attrs !! 1)
    let loc_e = snd (attrs !! 2)
    str <- xmlMatchString contents
    fail (show (loc_s,loc_e) ++ T.unpack str)

coqCall :: T.Text -> [Attrib] -> [Content] -> AppIO [Content]
coqCall val attrs contents = do
  value <- coqInteract $ Tag "call" (("val",val):attrs) contents
  xmlMatchTag' "value" [("val","good")] [value] (processFailure value)

fetchString :: Monad m => [Content] -> m T.Text
fetchString stringTag = do
  string <- xmlMatchTag "string" [] stringTag
  xmlMatchString string

fetchAbout :: Monad m => [Content] -> m CoqAbout
fetchAbout value = do
  coq_info <- xmlMatchTag "coq_info" [] value
  version <- fetchString [coq_info !! 0]
  v1 <- fetchString [coq_info !! 1]
  v2 <- fetchString [coq_info !! 2]
  v3 <- fetchString [coq_info !! 3]
  return $ CoqAbout {
    version = version,
    v1 = v1,
    v2 = v2,
    v3 = v3
  }

coqAbout :: AppIO CoqAbout
coqAbout = do
  xml <- coqCall "about" [] []
  fetchAbout xml

coqInterp :: T.Text -> AppIO T.Text
coqInterp text = do
  xml <- coqCall "interp" [("id","0"),("raw","")] [xmlString text]
  fetchString xml

coqCheck :: T.Text -> AppIO Expr
coqCheck text = do
  text <- coqInterp ("Check " `T.append` text `T.append` ".")
  coqParseTypeJudgement text

coqLookup :: T.Text -> AppIO CoqReport
coqLookup text = do
  about <- coqAbout
  check <- coqCheck text
  judgement <- unpackTop check
  (expr,typ) <- unpackTypeJudgement judgement
  return $ CoqReport {
    lookupExpr = text,
    expr = expr,
    typ = typ,
    about = about
  }

coqIndex :: AppIO CoqReport
coqIndex = do
  about <- coqAbout
  return $ CoqIndex { about = about }

coqChunks :: [T.Text] -> AppIO CoqReport
coqChunks [] = coqIndex
coqChunks [text] = do
  report <- coqLookup text
  return report
coqChunks _ = error "Too many path elements"
