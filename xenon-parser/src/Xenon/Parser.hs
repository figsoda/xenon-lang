{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Xenon.Parser (test) where

import Control.Monad.Combinators (between, choice, many, manyTill, sepEndBy, (<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Combinators.NonEmpty (sepBy1, some)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor (($>), (<&>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import GHC.Exts (fromList)
import Numeric.Natural (Natural)
import System.IO (hFlush, stdout)
import Text.Megaparsec (Parsec, ShowErrorComponent (..), anySingle, chunk, customFailure, eof, failure, notFollowedBy, oneOf, optional, parseTest, satisfy, try)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (binary, decimal, float, hexadecimal, octal, skipBlockComment, skipLineComment, space)
import Text.Megaparsec.Error (ErrorItem (..))

data Error
  = InvalidUnicodeEscape
  deriving (Eq, Ord)

instance ShowErrorComponent Error where
  showErrorComponent = \case
    InvalidUnicodeEscape -> "invalid unicode escape, unicode escape must be at most 0x10ffff"

type Parser = Parsec Error Text

data Expr
  = Int Integer
  | Nat Natural
  | Float Double
  | Char Char
  | String String
  | Var (NonEmpty String)
  | Unit
  | Tuple [Expr]
  | List [Expr]
  | App Expr [Expr]
  | Let Expr Expr Expr
  | Match Expr (NonEmpty (NonEmpty Expr, Maybe Expr, Expr))

instance Show Expr where
  show (Int x) = show x
  show (Nat x) = '+' : show x
  show (Float x) = show x
  show (Char x) = show x
  show (String x) = show x
  show (Var x) = foldl1 ((++) . (++ ".")) x
  show Unit = "()"
  show (Tuple x) = '(' : intercalate "," (map show x) ++ ")"
  show (List x) = show x
  show (App x xs) = '(' : intercalate " " (map show $ x : xs) ++ ")"
  show (Let pat val x) = "let " ++ show pat ++ " = " ++ show val ++ " in " ++ show x
  show (Match x xs) =
    "match " ++ show x ++ " with"
      ++ concatMap
        ( \(pats, guard, val) ->
            concatMap ((" | " ++) . show) pats ++ maybe "" ((" when " ++) . show) guard ++ " -> " ++ show val
        )
        xs

test :: IO ()
test = do
  putStr "> "
  hFlush stdout
  xs <- getLine
  parseTest (expr [[InfixL $ op "*", InfixL $ op "/"], [InfixL $ op "+", InfixL $ op "-"]] <* eof) (pack xs)
  test

ws :: Parser ()
ws = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

sym :: Parser Char
sym = oneOf ("!#$%&*+-/:<=>@\\^|" :: String)

op :: String -> Parser (Expr -> Expr -> Expr)
op xs = (try $ chunk (pack xs) <* notFollowedBy sym <* ws) $> \x y -> App (Var $ xs :| []) [x, y]

ident :: Parser String
ident = try $ do
  x <- satisfy $ \x -> isAsciiUpper x || isAsciiLower x
  xs <- many $ satisfy $ \y -> isDigit y || isAsciiUpper y || isAsciiLower y
  case (x : xs) of
    "in" -> ukw "in"
    "let" -> ukw "let"
    "match" -> ukw "match"
    "when" -> ukw "when"
    "with" -> ukw "with"
    ys -> pure ys
  where
    ukw :: String -> Parser String
    ukw xs =
      failure
        (Just $ Label $ fromList ("keyword " ++ show xs))
        (fromList [Label $ fromList "identifier"])

term :: [[Operator Parser Expr]] -> Parser Expr
term opss =
  choice
    [ try (sign <* (char '0' >> char 'B' <|> char 'b')) <*> binary <&> Int,
      try (sign <* (char '0' >> char 'O' <|> char 'o')) <*> octal <&> Int,
      try (sign <* (char '0' >> char 'X' <|> char 'x')) <*> hexadecimal <&> Int,
      try (sign <*> float) <&> Float,
      try (sign <*> decimal) <&> Int,
      between (char '\'') (char '\'') esc <&> Char,
      char '"' >> manyTill esc (char '"') <&> String,
      chunk "r\"" >> manyTill anySingle (char '"') <&> String,
      sepBy1 ident (char '.') <&> Var,
      between (char '(' <* ws) (char ')') (sepEndBy (expr opss) (char ',' <* ws)) <&> \case
        [] -> Unit
        [x] -> x
        xs -> Tuple xs,
      between (char '[' <* ws) (char ']') (sepEndBy (expr opss) (char ',' <* ws)) <&> List,
      do
        pat <- chunk "let" <* ws >> expr opss
        val <- char '=' <* ws >> expr opss
        chunk "in" <* ws >> expr opss <&> Let pat val,
      do
        val <- chunk "match" <* ws >> expr opss <* chunk "with" <* ws
        arms <- some $ do
          pat <- some $ char '|' <* ws >> expr opss
          guard <- optional $ chunk "when" <* ws >> expr opss
          chunk "->" <* ws >> expr opss <&> (pat,guard,)
        pure $ Match val arms
    ]
  where
    sign :: Num a => Parser (a -> a)
    sign = char '-' $> negate <|> pure id

    esc =
      char '\\'
        *> choice
          [ char '0' $> '\0',
            char 'n' $> '\n',
            char 'r' $> '\r',
            char 't' $> '\t',
            do
              x <- between (char '{') (char '}') hexadecimal
              if x < 0x110000 then pure $ toEnum x else customFailure InvalidUnicodeEscape,
            anySingle
          ]
        <|> anySingle

expr :: [[Operator Parser Expr]] -> Parser Expr
expr opss = makeExprParser (some (term opss <* ws) <&> app) opss
  where
    app (x :| []) = x
    app (x :| xs) = App x xs
