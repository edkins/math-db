{-# LANGUAGE OverloadedStrings #-}
module Expr where

import qualified Data.Text as T
import Data.Attoparsec.Text.Lazy
import Control.Applicative
import Data.Char

data Flavour = Var | Punctuation | Whitespace | TypeJudgement | Apply | Binop | Lambda | Paren | Top deriving Show
data Expr = Expr {
  exprFlavour :: Flavour,
  exprText :: T.Text,
  exprSub :: [Expr]
} deriving Show

coqParseTypeJudgement :: Monad m => T.Text -> m Expr
coqParseTypeJudgement text = errParse (asTop coqTypeJudgement) text

coqParseCompute :: Monad m => T.Text -> m Expr
coqParseCompute text = errParse (asComputeTop coqTypeJudgement) text

unpackTop :: Monad m => Expr -> m Expr
unpackTop (Expr Top text [_,e,_]) = return e
unpackTop e = fail ("Doesn't look like top-level thing: " ++ show e)

unpackTypeJudgement :: Monad m => Expr -> m (Expr, Expr)
unpackTypeJudgement (Expr TypeJudgement text [e,_,t]) = return (e,t)
unpackTypeJudgement e = fail ("Doesn't look like a type judgement: " ++ show e)

errParse :: Monad m => Parser Expr -> T.Text -> m Expr
errParse p text = 
  case parseOnly (p <* endOfInput) text of
    Left err -> fail ("Parse error: " ++ err)
    Right e -> return e

asTop :: Parser Expr -> Parser Expr
asTop p = combine Top [maybeSpace, p, maybeSpace]

asComputeTop :: Parser Expr -> Parser Expr
asComputeTop p = combine Top [punc "=", p, maybeSpace]

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
  e <- coqChain
  coqTryBinop e

coqTryBinop :: Expr -> Parser Expr
coqTryBinop e = coqBinop e <|> return e

coqBinop :: Expr -> Parser Expr
coqBinop e = do
  op <- atom Punctuation binopSpaces
  e' <- coqExpr
  let e_op_e' = makeExpr Binop [e, op, e']
  coqTryBinop e_op_e'

coqChain :: Parser Expr
coqChain = do
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
coqTerm = coqLambda <|> coqVar <|> coqParen

coqLambda :: Parser Expr
coqLambda = combine Lambda [punc "fun", coqVar, punc ":", coqExpr, punc "=>", coqExpr]

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

binopSpaces :: Parser T.Text
binopSpaces = do
  sp <- spaces
  str <- binop
  sp' <- spaces
  return (sp `T.append` str `T.append` sp')

binop = string "->" <|> string "+"

