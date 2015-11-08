{-# LANGUAGE OverloadedStrings #-}
module Expr where

import qualified Data.Text as T
import Data.Attoparsec.Text.Lazy
import Control.Applicative
import Data.Char

data Flavour = Var | Punctuation | Whitespace | TypeJudgement | Apply | Paren | Error String deriving Show
data Expr = Expr {
  exprFlavour :: Flavour,
  exprText :: T.Text,
  exprSub :: [Expr]
} deriving Show

coqParseTypeJudgement :: T.Text -> Expr
coqParseTypeJudgement text = errParse coqTypeJudgement text

unpackTypeJudgement :: Expr -> (Expr, Expr)
unpackTypeJudgement (Expr TypeJudgement text [e,_,t]) = (e,t)
unpackTypeJudgement e = error ("Doesn't look like a type judgement: " ++ (show e))

errParse :: Parser Expr -> T.Text -> Expr
errParse p text = 
  case parseOnly p text of
    Left err -> Expr {exprFlavour = Error err, exprText = text, exprSub = []}
    Right e -> e

makeExpr :: Flavour -> [Expr] -> Expr
makeExpr flavour es =
  let text = T.concat [exprText e | e <- es] in
  Expr {exprFlavour = flavour, exprText = text, exprSub = es}

makeExprForget :: Flavour -> [Expr] -> Expr
makeExprForget flavour es =
  let e = makeExpr flavour es in
  e {exprSub = []}

combine :: Flavour -> [Parser Expr] -> Parser Expr
combine flavour list = do
  es <- sequence list
  return (makeExpr flavour es)

combineForget :: Flavour -> [Parser Expr] -> Parser Expr
combineForget flavour list = do
  es <- sequence list
  return (makeExprForget flavour es)

atom :: Flavour -> Parser T.Text -> Parser Expr
atom flavour p = do
  str <- p
  return (Expr {exprFlavour = flavour, exprText = str, exprSub = []})

maybeSpace :: Parser Expr
maybeSpace = atom Whitespace spaces

punc :: T.Text -> Parser Expr
punc str = atom Punctuation (stringSpaces str)
  
coqTypeJudgement :: Parser Expr
coqTypeJudgement = combine TypeJudgement [coqExpr, punc ":", coqExpr]

coqExpr :: Parser Expr
coqExpr = do
  e <- coqTerm
  coqTryApply e

coqTryApply :: Expr -> Parser Expr
coqTryApply e = coqApply e <|> return e

coqApply :: Expr -> Parser Expr
coqApply f = do
  sp <- maybeSpace
  x <- coqTerm
  let fx = makeExpr Apply [f, sp, x]
  coqTryApply fx

coqTerm :: Parser Expr
coqTerm = coqVar <|> coqParen

coqVar :: Parser Expr
coqVar = atom Var word

coqParen :: Parser Expr
coqParen = combine Paren [punc "(", coqExpr, punc ")"]

word :: Parser T.Text
word = takeWhile1 isAlphaNum_'

isAlpha_ c = isAlpha c || c == '_'
isAlphaNum_' c = isAlphaNum c || c == '_' || c == '\''

spaces :: Parser T.Text
spaces = Data.Attoparsec.Text.Lazy.takeWhile isSpace

stringSpaces :: T.Text -> Parser T.Text
stringSpaces str = do
  sp <- spaces
  str <- string str
  sp' <- spaces
  return (sp `T.append` str `T.append` sp')
