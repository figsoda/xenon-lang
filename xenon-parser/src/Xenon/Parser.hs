{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Xenon.Parser ( Parser, expr, fndef, ident, term, test ) where

import Control.Applicative ( liftA2 )
import Control.Monad.Combinators
  ( (<|>), choice, many, manyTill, optional, sepEndBy, skipMany )
import Control.Monad.Combinators.Expr ( Operator(..), makeExprParser )
import Control.Monad.Combinators.NonEmpty ( sepBy1, some )
import Data.Char ( isAsciiLower, isAsciiUpper, isDigit )
import Data.Functor ( ($>), (<&>) )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Text ( Text, pack, unpack )
import GHC.Exts ( fromList )
import Numeric.Natural ( Natural )
import Text.Megaparsec
  ( Parsec, ShowErrorComponent(..), anySingle, chunk, customFailure, eof
  , failure, hidden, notFollowedBy, oneOf, satisfy, takeP, takeWhileP, try )
import Text.Megaparsec.Char ( char, space, space1 )
import Text.Megaparsec.Char.Lexer ( binary, decimal, float, hexadecimal, octal )
import Text.Megaparsec.Error ( ErrorItem(..) )
import Xenon.Ast ( Expr(..), FnDef(..) )

data ParseError = InvalidUnicodeEscape
  deriving ( Eq, Ord )

instance ShowErrorComponent ParseError where
  showErrorComponent = \case
    InvalidUnicodeEscape
     -> "invalid unicode escape, unicode escape must be at most 0x10ffff"

type Parser = Parsec ParseError Text

infixl 5 <@>

(<@>) :: Applicative f => f a -> f b -> f ()
(<@>) = liftA2 (\_ _ -> ())

test :: Parser Expr
test
  = expr
      [ [ InfixL $ op "*", InfixL $ op "/" ]
      , [ InfixL $ op "+", InfixL $ op "-" ]
      ]
  <* eof

ws :: Parser ()
ws
  = hidden
  $ skipMany
  $ space1
  <|> syms "//" <@> takeWhileP Nothing (/= '\n')
  <|> syms "/*" <@> manyTill anySingle "*/"

sym :: Parser Char
sym = oneOf ("!#$%&*+-/:<=>@\\^|" :: String)

syms :: Text -> Parser Text
syms xs = try $ chunk xs <* notFollowedBy sym

op :: String -> Parser (Expr -> Expr -> Expr)
op xs = syms (pack xs) <* ws $> App . App (Var xs [])

ident :: Parser String
ident = try $ do
  x <- satisfy $ \x -> x == '_' || isAsciiUpper x || isAsciiLower x
  xs <- many $ satisfy $ \y ->
    y == '_' || isDigit y || isAsciiUpper y || isAsciiLower y
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
      = failure
          (Just $ Label $ fromList ("keyword " ++ show xs))
          (fromList [ Label $ fromList "identifier" ])

term :: [[Operator Parser Expr]] -> Parser Expr
term opss
  = choice
      [ try (signedInt <* (char '0' >> char 'B' <|> char 'b')) <*> binary
      , try (signedInt <* (char '0' >> char 'O' <|> char 'o')) <*> octal
      , try (signedInt <* (char '0' >> char 'X' <|> char 'x')) <*> hexadecimal
      , try (sign (Float . negate) Float <*> float)
      , try (signedInt <*> decimal)
      , do
          char '\''
          x <- char '\\'
            *> choice
                [ char '0' $> '\0'
                , char 'n' $> '\n'
                , char 'r' $> '\r'
                , char 't' $> '\t'
                , uesc
                , anySingle
                ]
            <|> anySingle
          char '\''
          pure $ Char x
      , char '"'
          >> manyTill
              (char '\\'
               *> choice
                   [ char '0' $> '\0'
                   , char 'n' $> '\n'
                   , char 'r' $> '\r'
                   , char 't' $> '\t'
                   , char '\n' >> space $> '\n'
                   , uesc
                   , anySingle
                   ]
               <|> anySingle)
              (char '"')
          <&> String
      , chunk "r\"" >> takeWhileP Nothing (/= '"') <* takeP Nothing 1
          <&> String . unpack
      , sepBy1 ident (char '.') <&> \(x :| xs) -> Var x xs
      , do
          char '(' <* ws
          xs <- sepEndBy (expr opss) (char ',' <* ws)
          char ')'
          pure $ case xs of
            [] -> Unit
            [x] -> x
            _ -> foldr1 Pair xs
      , do
          char '[' <* ws
          xs <- sepEndBy (expr opss) (char ',' <* ws)
          char ']'
          pure $ List xs
      , do
          chunk "let" <* ws
          xs <- flip sepEndBy (char ',' <* ws) $ do
            pat <- expr opss
            syms "=" <* ws
            val <- expr opss
            pure (pat, val)
          x <- chunk "in" <* ws >> expr opss
          pure $ Let xs x
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

    signedInt :: Parser (Natural -> Expr)
    signedInt = sign (Int . negate . toInteger) Nat

    uesc :: Parser Char
    uesc = do
      x <- char '{' >> hexadecimal <* char '}'
      if x < 0x110000
        then pure $ toEnum x
        else customFailure InvalidUnicodeEscape

expr :: [[Operator Parser Expr]] -> Parser Expr
expr opss = makeExprParser (some (term opss <* ws) <&> foldl1 App) opss

fndef :: [[Operator Parser Expr]] -> FnDef
fndef = undefined
