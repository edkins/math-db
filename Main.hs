{-# LANGUAGE OverloadedStrings #-}
module Main where

{- http://www.yesodweb.com/book/web-application-interface -}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Network.Wai.Dispatch
import Network.Wai.Application.Static
import Data.ByteString.Lazy.Char8 (pack)

content_type_text_plain = [("Content-Type", "text/plain")]
content_type_text_html = [("Content-Type", "text/html")]

error404 :: Application
error404 request respond =
  respond $ responseLBS
    status404
    content_type_text_plain
    (pack ("Page not found: " ++ show (rawPathInfo request)))

indexPage :: Application
indexPage request respond =
  respond $ responseLBS
    status200
    content_type_text_html
    "This is the math-db application."

coq :: Application
coq request respond =
  respond $ responseLBS
    status500
    content_type_text_plain
    "Not implemented yet"

app :: Application
app request respond =
  let path = pathInfo request in
  if path == [] then
    indexPage request respond
  else if head path == "coq" then
    coq request respond
  else
    error404 request respond

static_middleware :: Middleware
static_middleware = staticPolicy (hasPrefix "static/")

main :: IO ()
main = do
    putStrLn "http://localhost:8080/"
    run 8080 $ static_middleware app
