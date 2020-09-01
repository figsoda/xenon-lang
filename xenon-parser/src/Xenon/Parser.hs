{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Xenon.Parser ( test ) where

import Control.Applicative ( liftA2 )
import Control.Monad.Combinators
  ( (<|>), between, choice, many, manyTill, optional, sepEndBy )
import Control.Monad.Combinators.Expr ( Operator(..), makeExprParser )
import Control.Monad.Combinators.NonEmpty ( sepBy1, some )
import Data.Char ( isAsciiLower, isAsciiUpper, isDigit )
import Data.Functor ( ($>), (<&>) )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Text ( Text, pack )
import GHC.Exts ( fromList )
import Numeric.Natural ( Natural )
import System.IO ( hFlush, stdout )
import Text.Megaparsec
  ( Parsec, ShowErrorComponent(..), anySingle, chunk, customFailure, eof
  , failure, notFollowedBy, oneOf, parseTest, satisfy, takeWhileP, try )
import Text.Megaparsec.Char ( char, space1 )
import Text.Megaparsec.Char.Lexer
  ( binary, decimal, float, hexadecimal, octal, space )
import Text.Megaparsec.Error ( ErrorItem(..) )
import Xenon.Ast ( Expr(..) )

data ParseError = InvalidUnicodeEscape
  deriving ( Eq, Ord )

instance ShowErrorComponent ParseError where
  showErrorComponent = \case
    InvalidUnicodeEscape ->
      "invalid unicode escape, unicode escape must be at most 0x10ffff"

type Parser = Parsec ParseError Text

infixl 1 <@>

(<@>) :: Applicative f => f a -> f b -> f ()
(<@>) = liftA2 (\_ _ -> ())

test :: IO ()
test = do
  putStr "> "
  hFlush stdout
  xs <- getLine
  parseTest (expr [ [ InfixL $ op "*", InfixL $ op "/" ]
                  , [ InfixL $ op "+", InfixL $ op "-" ]
                  ]
             <* eof) (pack xs)
  test

ws :: Parser ()
ws
  = space space1 (syms "//" <@> takeWhileP Nothing (/= '\n'))
  (syms "/*" <@> manyTill anySingle "*/")

sym :: Parser Char
sym = oneOf ("!#$%&*+-/:<=>@\\^|" :: String)

syms :: Text -> Parser Text
syms xs = try $ chunk xs <* notFollowedBy sym

op :: String -> Parser (Expr -> Expr -> Expr)
op xs = syms (pack xs) <* ws $> App . App (Var xs [])

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
    ukw xs
      = failure (Just $ Label $ fromList ("keyword " ++ show xs))
      (fromList [ Label $ fromList "identifier" ])

term :: [[Operator Parser Expr]] -> Parser Expr
term opss
  = choice
  [ try (signedInt <* (char '0' >> char 'B' <|> char 'b')) <*> binary
  , try (signedInt <* (char '0' >> char 'O' <|> char 'o')) <*> octal
  , try (signedInt <* (char '0' >> char 'X' <|> char 'x')) <*> hexadecimal
  , try (signed Float <*> float)
  , try (signedInt <*> decimal)
  , between (char '\'') (char '\'') esc <&> Char
  , char '"' >> manyTill esc (char '"') <&> String
  , chunk "r\"" >> manyTill anySingle (char '"') <&> String
  , sepBy1 ident (char '.') <&> \(x :| xs) -> Var x xs
  , between (char '(' <* ws) (char ')') (sepEndBy (expr opss) (char ',' <* ws))
      <&> \case
        [] -> Unit
        [x] -> x
        xs -> foldr1 Pair xs
  , between (char '[' <* ws) (char ']') (sepEndBy (expr opss) (char ',' <* ws))
      <&> List
  , do
      pat <- chunk "let" <* ws >> expr opss
      val <- syms "=" <* ws >> expr opss
      x <- chunk "in" <* ws >> expr opss
      pure $ Let pat val x
  , do
      cond <- chunk "if" <* ws >> expr opss
      x <- chunk "then" <* ws >> expr opss
      y <- chunk "else" <* ws >> expr opss
      pure $ If cond x y
  , do
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

    esc
      = char '\\'
      *> choice
      [ char '0' $> '\0'
      , char 'n' $> '\n'
      , char 'r' $> '\r'
      , char 't' $> '\t'
      , do
          x <- between (char '{') (char '}') hexadecimal
          if x < 0x110000
            then pure $ toEnum x
            else customFailure InvalidUnicodeEscape
      , anySingle
      ]
      <|> anySingle

expr :: [[Operator Parser Expr]] -> Parser Expr
expr opss = makeExprParser (some (term opss <* ws) <&> foldl1 App) opss
