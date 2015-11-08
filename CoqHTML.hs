{-# LANGUAGE OverloadedStrings #-}
module CoqHTML where

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Coq
import AppIO
import Expr

logEntryHTML :: (LogEntry, T.Text) -> H.Html
logEntryHTML (entry,text) = H.p $ H.toHtml text

logHTML :: Log -> H.Html
logHTML log = sequence_ (map logEntryHTML log)

coqHTML :: CoqReport -> Log -> H.Html
coqHTML report log = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ exprText $ expr report
  H.body $ do
    H.h1 $ H.toHtml $ exprText $ expr report
    H.p $ H.toHtml $ lookupExpr report
    H.p $ H.toHtml $ exprText $ typ report
    H.p $ H.toHtml $ about report
    logHTML log
