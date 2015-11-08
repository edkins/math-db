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
typeHTML t = p $ do
  H.span "Type: "
  a ! A.href (toValue ("/coq/" `T.append` exprText t)) $ do
    toHtml (exprText t)

computedHTML e = do
  h2 "Computed value"
  p $ toHtml $ exprText e

aboutHTML :: CoqAbout -> Html
aboutHTML a = p $ toHtml $ version a

coqHTML :: Either T.Text CoqReport -> Log -> Html

coqHTML (Left err) log =
  docTypeHtml $ do
    H.head $ do
      title "error"
    body $ do
      p $ toHtml err
      hr
      logHTML log

coqHTML (Right report) log =
  let e = expr report in
  docTypeHtml $ do
    H.head $ do
      title $ toHtml $ exprText $ e
    body $ do
      headingHTML e
      redirectHTML (lookupExpr report) e
      typeHTML (typ report)
      computedHTML (computed report)
      hr
      logHTML log
      hr
      aboutHTML (about report)
