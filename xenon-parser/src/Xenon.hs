{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Xenon (test) where

import Control.Monad.Combinators (between, choice, many, sepBy1, sepEndBy, some, (<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor (($>), (<&>))
import Data.List (intercalate)
import Data.Text (Text, pack)
import Data.Void (Void)
import Numeric.Natural (Natural)
import System.IO (hFlush, stdout)
import Text.Megaparsec (Parsec, anySingle, anySingleBut, chunk, eof, parseTest, satisfy, try)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (binary, decimal, float, hexadecimal, octal, skipBlockComment, skipLineComment, space)

type Parser = Parsec Void Text

data Expr
  = Int Integer
  | Nat Natural
  | Float Double
  | Char Char
  | String String
  | Var [String]
  | Unit
  | Tuple [Expr]
  | List [Expr]
  | Call Expr [Expr]

instance Show Expr where
  show (Int x) = show x
  show (Nat x) = '+' : show x
  show (Float x) = show x
  show (Char x) = show x
  show (String x) = show x
  show (Var x) = intercalate "." x
  show Unit = "Unit"
  show (Tuple x) = '(' : intercalate "," (map show x) ++ ")"
  show (List x) = show x
  show (Call x y) = '(' : show x ++ ' ' : show y ++ ")"

test :: IO ()
test = do
  putStr "> "
  hFlush stdout
  xs <- getLine
  parseTest (expr [[InfixL $ op "*", InfixL $ op "/"], [InfixL $ op "+", InfixL $ op "-"]] <* eof) (pack xs)
  test

ws :: Parser ()
ws = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

op :: String -> Parser (Expr -> Expr -> Expr)
op xs = (chunk (pack xs) <* ws) $> \x y -> Call (Var [xs]) [x, y]

expr :: [[Operator Parser Expr]] -> Parser Expr
expr opss = makeExprParser (some (term <* ws) <&> \(x : xs) -> Call x xs) opss
  where
    term =
      choice
        [ try (sign <* (char '0' >> char 'B' <|> char 'b')) <*> binary <&> Int,
          try (sign <* (char '0' >> char 'O' <|> char 'o')) <*> octal <&> Int,
          try (sign <* (char '0' >> char 'X' <|> char 'x')) <*> hexadecimal <&> Int,
          try (sign <*> float) <&> Float,
          try (sign <*> decimal) <&> Int,
          between (char '\'') (char '\'') (anySingleBut '\\' <|> esc) <&> Char,
          between (char '"') (char '"') (many $ satisfy (\x -> x /= '\\' && x /= '"') <|> esc) <&> String,
          between (chunk "r\"") (char '"') (many $ anySingleBut '"') <&> String,
          sepBy1 ((:) <$> satisfy alpha <*> many (satisfy $ \x -> alpha x || isDigit x)) (char '.') <&> Var,
          between (char '(' <* ws) (char ')') (sepEndBy (expr opss <* ws) (char ',' <* ws)) <&> \case
            [] -> Unit
            [x] -> x
            xs -> Tuple xs,
          between (char '[' <* ws) (char ']') (sepEndBy (expr opss <* ws) (char ',' <* ws)) <&> List
        ]

    sign :: Num a => Parser (a -> a)
    sign = char '-' $> negate <|> pure id

    esc =
      char '\\'
        >> choice
          [ char '0' $> '\0',
            char 'n' $> '\n',
            char 'r' $> '\r',
            char 't' $> '\t',
            do
              x <- between (char '{') (char '}') hexadecimal
              if x < 0x110000 then pure $ toEnum x else fail "invalid unicode character escape",
            anySingle
          ]

    alpha x = isAsciiUpper x || isAsciiLower x
