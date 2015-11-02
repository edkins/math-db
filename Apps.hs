{-# LANGUAGE OverloadedStrings #-}
module Apps where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Network.Wai.Dispatch
import Network.Wai.Application.Static
import Data.ByteString.Lazy.Char8 (pack)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import IndexPage

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


coq :: Application
coq request respond =
  respond $ responseLBS
    status500
    content_type_text_plain
    "Not implemented yet"

