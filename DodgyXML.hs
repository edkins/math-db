{-# LANGUAGE OverloadedStrings #-}
module DodgyXML where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Attoparsec.Text.Lazy
import Control.Applicative
import Data.Char

type Attrib = (T.Text,T.Text)
data Content = Tag T.Text [Attrib] [Content] | String T.Text | Character T.Text deriving Show

{-
 - A totally broken non-conformant XML parser, sufficient for parsing the output of coqtop -ideslave
 - The reason to use this rather than Text.XML.Light is that it needs to notice as soon
 - as the end of a tag is reached.
 -}


parseItem = parseTagged <|> parseString <|> parseCharacter

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

parseCharacter = do
  char '&'
  result <- takeTill (';'==)
  char ';'
  return $ Character result

isStringChar ch = (ch /= '<' && ch /= '&')
isWordChar ch = (isAlpha ch || ch == '_')

parseString = do
  text <- takeWhile1 isStringChar
  return $ String text

showAttrs :: [Attrib] -> T.Text
showAttrs [] = ""
showAttrs ((k,v):attrs) = " " `T.append` k `T.append` "=\"" `T.append` v `T.append` "\"" `T.append` showAttrs attrs

showContent :: Content -> T.Text
showContent (String str) = str
showContent (Character str) = "&" `T.append` str `T.append` ";"
showContent (Tag name attrs []) = "<" `T.append` name `T.append` showAttrs attrs `T.append` "/>"
showContent (Tag name attrs contents) = "<" `T.append` name `T.append` showAttrs attrs `T.append` ">" `T.append` showContents contents `T.append` "</" `T.append` name `T.append` ">"

showContents :: [Content] -> T.Text
showContents [] = ""
showContents (c:cs) = showContent c `T.append` showContents cs

xmlMatchTag' :: Monad m => T.Text -> [Attrib] -> [Content] -> m [Content] -> m [Content]
xmlMatchTag' name attrs [Tag name' attrs' contents] fallback =
  if name /= name' then
    fallback
  else if attrs /= attrs' then
    fallback
  else
    return contents
xmlMatchTag' _ _ _ fallback = fallback

xmlMatchTag :: Monad m => T.Text -> [Attrib] -> [Content] -> m [Content]
xmlMatchTag name attrs [Tag name' attrs' contents] =
  if name /= name' then
    fail "Name mismatch"
  else if attrs /= attrs' then
    fail "Attr mismatch"
  else
    return contents
xmlMatchTag name _ _ = fail ("Expecting single tag:" ++ T.unpack name)

decodeEntity :: T.Text -> T.Text
decodeEntity ent =
  if ent == "lt" then "<"
  else if ent == "gt" then ">"
  else if ent == "amp" then "&"
  else if ent == "quot" then "\""
  else if ent == "apos" then "'"
  else error ("Unknown entity:" ++ T.unpack ent)

xmlMatchOneString :: Monad m => Content -> m T.Text
xmlMatchOneString (String text) = return text
xmlMatchOneString (Character ent) = return $ decodeEntity ent
xmlMatchOneString _ = fail "Expecting text"

xmlMatchString :: Monad m => [Content] -> m T.Text
xmlMatchString cs = do
  stuff <- sequence (map xmlMatchOneString cs)
  return (T.concat stuff)

xmlString :: T.Text -> Content
xmlString str = String (
  T.replace "<" "&lt;" $
  T.replace ">" "&gt;" $
  T.replace "&" "&amp;" str)
