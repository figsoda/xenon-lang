module Xenon.Ast ( Expr(..), Fn(..) ) where

import Data.List ( intercalate )
import Data.List.NonEmpty ( NonEmpty(..) )
import Numeric.Natural ( Natural )
import Text.Printf ( printf )

data Expr
  = Int Integer
  | Nat Natural
  | Float Double
  | Char Char
  | String String
  | Var [String] String
  | Unit
  | Pair Expr Expr
  | List [Expr]
  | App Expr Expr
  | Let [(Expr, Expr)] Expr
  | If Expr Expr Expr
  | Match Expr (NonEmpty (NonEmpty Expr, Maybe Expr, Expr))
  | Arrow Expr Expr
  | Context String [String] String [Expr] Expr

showArm :: Show a => (NonEmpty a, Maybe Expr, Expr) -> String
showArm (pats, guard, val)
  = printf
      "\n%s%s => %s"
      (concatMap (("| " ++) . show) pats)
      (maybe "" ((" when " ++) . show) guard)
      (show val)

instance Show Expr where
  show (Int x) = show x
  show (Nat x) = '+' : show x
  show (Float x) = show x
  show (Char x) = show x
  show (String x) = show x
  show (Var xs x) = concatMap (++ ".") xs ++ x
  show Unit = "()"
  show (Pair x y) = printf "(%s,%s)" (show x) (show y)
  show (List x) = show x
  show (App x y) = printf "(%s %s)" (show x) (show y)
  show (Let xs x) = printf "let %s in %s" (intercalate ", " ys) (show x)
    where
      ys = map (\(pat, val) -> show pat ++ " = " ++ show val) xs
  show (If cond x y)
    = printf "if %s then %s else %s" (show cond) (show x) (show y)
  show (Match x xs) = "match " ++ show x ++ concatMap showArm xs
  show (Arrow x y) = show x ++ " -> " ++ show y
  show (Context name path trait args x)
    = printf
        "%s : %s%s%s => %s"
        name
        (concatMap (++ ".") path)
        trait
        (concatMap ((' ' :) . show) args)
        (show x)

data Fn = Fn Expr (NonEmpty (NonEmpty [Expr], Maybe Expr, Expr))

instance Show Fn where
  show (Fn ty xs) = show ty ++ concatMap showArm xs
