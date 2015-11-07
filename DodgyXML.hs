{-# LANGUAGE OverloadedStrings #-}
module DodgyXML where

import Data.Text
import qualified Data.Text.Lazy as L
import Data.Attoparsec.Text.Lazy
import Control.Applicative
import Data.Char
import Debug.Trace

type Attrib = (Text,Text)
data Content = Tag Text [Attrib] [Content] | String Text deriving Show

{-
 - A totally broken non-conformant XML parser, sufficient for parsing the output of coqtop -ideslave
 - The reason to use this rather than Text.XML.Light is that it needs to notice as soon
 - as the end of a tag is reached.
 -}

parseXML :: L.Text -> [Content]
parseXML input =
  case parse parseItem input of
    Done input' r -> r : parseXML input'
    res -> trace (show res) []

parseItem = parseTagged <|> parseString

parseTagged :: Parser Content
parseTagged = do
  char '<'
  name <- parseWord
  attrs <- many parseAttr
  (do
    char '>'
    contents <- many parseItem
    char '<'
    char '/'
    endname <- parseWord
    char '>'
    return $ Tag name attrs contents
    ) <|> (do
    char '/'
    char '>'
    return $ Tag name attrs [])

parseWord = takeWhile1 isWordChar

parseAttr = do
  char ' '
  key <- parseWord
  char '='
  value <- parseQuoted
  return (key,value)

parseQuoted = do
  char '"'
  result <- takeTill ('"'==)
  char '"'
  return result

isStringChar ch = (ch /= '<' && ch /= '&')
isWordChar ch = (isAlpha ch || ch == '_')

parseString = do
  text <- takeWhile1 isStringChar
  return $ String text

showAttrs :: [Attrib] -> Text
showAttrs [] = ""
showAttrs ((k,v):attrs) = " " `append` k `append` "=\"" `append` v `append` "\"" `append` showAttrs attrs

showContent :: Content -> Text
showContent (String str) = str
showContent (Tag name attrs []) = "<" `append` name `append` showAttrs attrs `append` "/>"
showContent (Tag name attrs contents) = "<" `append` name `append` showAttrs attrs `append` ">" `append` showContents contents `append` "</" `append` name `append` ">"

showContents :: [Content] -> Text
showContents [] = ""
showContents (c:cs) = showContent c `append` showContents cs

