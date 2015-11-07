{-# LANGUAGE OverloadedStrings #-}
module Apps where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Network.Wai.Dispatch
import Network.Wai.Application.Static
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Text (append)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import IndexPage
import Services
import Coq

content_type_text_plain = [("Content-Type", "text/plain")]
content_type_text_html = [("Content-Type", "text/html")]

error404 :: Application
error404 request respond =
  respond $ responseLBS
    status404
    content_type_text_plain
    (pack ("Page not found: " ++ show (rawPathInfo request)))

indexPageApp :: Application
indexPageApp request respond =
  let str = renderHtml (indexPageAsHTML IndexPage) in
  respond $ responseLBS
    status200
    content_type_text_html
    str


coq :: CoqProcess -> Application
coq c request respond =
  let chunks = pathInfo request in
  if length chunks == 1 then do
    message <- coqGetVersion c
    respond $ responseLBS status200 content_type_text_html (encodeUtf8 $ fromStrict message)
  else if length chunks == 2 then do
    expr <- asCoqExpr (chunks !! 1)
    (expr', typeString) <- coqGetType c expr
    let message = append expr' typeString
    let message' = encodeUtf8 $ fromStrict message
    respond $ responseLBS status200 content_type_text_html message'
  else
    respond $ responseLBS
      status500
      content_type_text_plain
      "Wrong number of chunks"

