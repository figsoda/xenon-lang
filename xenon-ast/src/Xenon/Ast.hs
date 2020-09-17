module Xenon.Ast ( Expr(..), Def(..) ) where

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
  | Var String [String]
  | Unit
  | Pair Expr Expr
  | List [Expr]
  | App Expr Expr
  | Let [(Expr, Expr)] Expr
  | If Expr Expr Expr
  | Match Expr (NonEmpty (NonEmpty Expr, Maybe Expr, Expr))
  | Arrow Expr Expr
  | Context String String [Expr] Expr

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
  show (Let xs x)
    = printf
        "let %s in %s"
        (intercalate ", "
         $ map (\(pat, val) -> printf "%s = %s" (show pat) (show val)) xs)
        (show x)
  show (If cond x y)
    = printf "if %s then %s else %s" (show cond) (show x) (show y)
  show (Match x xs) = "match " ++ show x ++ concatMap showArm xs
    where
      showArm (pats, guard, val)
        = printf
            "\n%s%s => %s"
            (concatMap (("| " ++) . show) pats)
            (maybe "" ((" when " ++) . show) guard)
            (show val)
  show (Arrow x y) = show x ++ " -> " ++ show y
  show (Context name trait args x)
    = printf
        "%s : %s%s => %s"
        name
        trait
        (concatMap ((' ' :) . show) args)
        (show x)

data Def = Def Expr (NonEmpty (NonEmpty [Expr], Maybe Expr, Expr))

instance Show Def where
  show (Def ty xs) = show ty ++ concatMap showArm xs
    where
      showArm (pats, guard, val)
        = printf
            "\n%s%s => %s"
            (concatMap (("| " ++) . show) pats)
            (maybe "" ((" when " ++) . show) guard)
            (show val)
