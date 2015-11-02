{-# LANGUAGE OverloadedStrings #-}
module IndexPage where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

data IndexPage = IndexPage

indexPageAsHTML :: IndexPage -> Html
indexPageAsHTML IndexPage = html $ do
  H.head $ do
    H.title "Math-db"
  body $ do
    "This is the Math-db."

