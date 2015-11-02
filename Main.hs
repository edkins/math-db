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

import Apps

app :: Application
app request respond =
  let path = pathInfo request in
  if path == [] then
    indexPageApp request respond
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
