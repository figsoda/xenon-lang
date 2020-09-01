module Xenon.Ast ( Expr(..) ) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Numeric.Natural ( Natural )
import Text.Printf ( printf )

data Expr
  = Int Integer
  | Nat Natural
  | Float Double
  | Char Char
  | String String
  | Var String [String]
  | Unit
  | Pair Expr Expr
  | List [Expr]
  | App Expr Expr
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
  show (Pair x y) = printf "(%s,%s)" (show x) (show y)
  show (List x) = show x
  show (App x y) = printf "(%s %s)" (show x) (show y)
  show (Let pat val x)
    = printf "let %s = %s in %s" (show pat) (show val) (show x)
  show (If cond x y)
    = printf "if %s then %s else %s" (show cond) (show x) (show y)
  show (Match x xs) = "match " ++ show x ++ concatMap showArm xs
    where
      showArm (pats, guard, val)
        = printf "\n%s%s -> %s" (concatMap (("| " ++) . show) pats)
        (maybe "" ((" when " ++) . show) guard) (show val)
