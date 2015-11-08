{-# LANGUAGE OverloadedStrings #-}
module CoqHTML where

import qualified Data.Text as T
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Coq
import AppIO
import Expr

logEntryHTML :: (LogEntry, T.Text) -> Html
logEntryHTML (entry,text) = p $ toHtml text

logHTML :: Log -> Html
logHTML log = sequence_ (Prelude.map logEntryHTML log)

headingHTML :: Expr -> Html
headingHTML e = h1 $ toHtml $ exprText e

redirectHTML :: T.Text -> Expr -> Html
redirectHTML orig changed
  | orig /= exprText changed =
    p $ toHtml ("Redirected from: " `T.append` orig)
  | otherwise =
    return ()

typeHTML :: Expr -> Html
typeHTML t = p $ toHtml $ ("Type: " `T.append` exprText t)

aboutHTML :: CoqAbout -> Html
aboutHTML a = p $ toHtml $ version a

coqHTML :: CoqReport -> Log -> Html
coqHTML report log =
  let e = expr report in
  docTypeHtml $ do
    H.head $ do
      title $ toHtml $ exprText $ e
    body $ do
      headingHTML e
      redirectHTML (lookupExpr report) e
      typeHTML (typ report)
      hr
      logHTML log
      hr
      aboutHTML (about report)
