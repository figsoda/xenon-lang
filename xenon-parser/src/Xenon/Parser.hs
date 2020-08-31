{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Text.Printf (printf)

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
  | Var String [String]
  | Unit
  | Tuple Expr Expr [Expr]
  | List [Expr]
  | App Expr [Expr]
  | Let Expr Expr Expr
  | If Expr Expr Expr
  | Match Expr (NonEmpty (NonEmpty Expr, Maybe Expr, Expr))

instance Show Expr where
  show (Int x) = show x
  show (Nat x) = '+' : show x
  show (Float x) = show x
  show (Char x) = show x
  show (String x) = show x
  show (Var x xs) = foldl ((++) . (++ ".")) x xs
  show Unit = "()"
  show (Tuple x y xs) = printf "(%s,%s,%s)" (show x) (show y) (intercalate "," $ map show xs)
  show (List x) = show x
  show (App x xs) = '(' : unwords (map show $ x : xs) ++ ")"
  show (Let pat val x) = printf "let %s = %s in %s" (show pat) (show val) (show x)
  show (If cond x y) = printf "if %s then %s else %s" (show cond) (show x) (show y)
  show (Match x xs) =
    "match " ++ show x
      ++ concatMap
        ( \(pats, guard, val) -> printf "\n%s%s -> %s"
            (concatMap (("| " ++) . show) pats)
            (maybe "" ((" when " ++) . show) guard)
            (show val)
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

syms :: Text -> Parser Text
syms xs = try $ chunk xs <* notFollowedBy sym

op :: String -> Parser (Expr -> Expr -> Expr)
op xs = syms (pack xs) <* ws $> \x y -> App (Var xs []) [x, y]

ident :: Parser String
ident = try $ do
  x <- satisfy $ \x -> isAsciiUpper x || isAsciiLower x
  xs <- many $ satisfy $ \y -> isDigit y || isAsciiUpper y || isAsciiLower y
  case x : xs of
    "else" -> ukw "else"
    "if" -> ukw "if"
    "in" -> ukw "in"
    "let" -> ukw "let"
    "match" -> ukw "match"
    "then" -> ukw "then"
    "when" -> ukw "when"
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
    [ try (signedInt <* (char '0' >> char 'B' <|> char 'b')) <*> binary,
      try (signedInt <* (char '0' >> char 'O' <|> char 'o')) <*> octal,
      try (signedInt <* (char '0' >> char 'X' <|> char 'x')) <*> hexadecimal,
      try (signed Float <*> float),
      try (signedInt <*> decimal),
      between (char '\'') (char '\'') esc <&> Char,
      char '"' >> manyTill esc (char '"') <&> String,
      chunk "r\"" >> manyTill anySingle (char '"') <&> String,
      sepBy1 ident (char '.') <&> \(x :| xs) -> Var x xs,
      between (char '(' <* ws) (char ')') (sepEndBy (expr opss) (char ',' <* ws)) <&> \case
        [] -> Unit
        [x] -> x
        (x : y : xs) -> Tuple x y xs,
      between (char '[' <* ws) (char ']') (sepEndBy (expr opss) (char ',' <* ws)) <&> List,
      do
        pat <- chunk "let" <* ws >> expr opss
        val <- syms "=" <* ws >> expr opss
        x <- chunk "in" <* ws >> expr opss
        pure $ Let pat val x,
      do
        cond <- chunk "if" <* ws >> expr opss
        x <- chunk "then" <* ws >> expr opss
        y <- chunk "else" <* ws >> expr opss
        pure $ If cond x y,
      do
        val <- chunk "match" <* ws >> expr opss
        arms <- some $ do
          pat <- some $ syms "|" <* ws >> expr opss
          guard <- optional $ chunk "when" <* ws >> expr opss
          x <- syms "->" <* ws >> expr opss
          pure (pat, guard, x)
        pure $ Match val arms
    ]
  where
    sign :: Num a => (a -> b) -> (a -> b) -> Parser (a -> b)
    sign f g = char '-' $> f <|> pure g

    signed :: Num a => (a -> b) -> Parser (a -> b)
    signed f = sign (f . negate) f

    signedInt :: Parser (Natural -> Expr)
    signedInt = sign (Int . negate . toInteger) Nat

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
