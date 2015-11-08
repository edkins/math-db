{-# LANGUAGE OverloadedStrings #-}
module Apps where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Network.Wai.Dispatch
import Network.Wai.Application.Static
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import IndexPage
import Services
import Coq
import AppIO

content_type_text_plain = [("Content-Type", "text/plain")]
content_type_text_html = [("Content-Type", "text/html")]

error404 :: Application
error404 request respond =
  respond $ responseLBS
    status404
    content_type_text_plain
    (B.pack ("Page not found: " ++ show (rawPathInfo request)))

indexPageApp :: Application
indexPageApp request respond =
  let str = renderHtml (indexPageAsHTML IndexPage) in
  respond $ responseLBS
    status200
    content_type_text_html
    str


coq :: Processes -> Application
coq p request respond =
  let (_:chunks) = pathInfo request in
  let appio = coqChunks chunks in do
    (result, log) <- runAppIO p appio
    let message = result `T.append` T.pack (show log)
    respond $ responseLBS status200 content_type_text_html (L.encodeUtf8 $ L.fromStrict message)
