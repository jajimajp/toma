module ParserTerm where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Data.List as List

-- terms only for parser
type Var = String
type Function = String

data Term = V Var
          | F Function [Term]
          deriving Eq

showTerm :: Term -> BSB.Builder
showTerm (V x) = BSB.string7 x
showTerm (F f ts) = BSB.string7 f <> BSB.string7 "(" <> mconcat (List.intersperse (BSB.string7 ", ") [ showTerm t | t <- ts]) <> BSB.string7 ")"

type TermPair = (Term, Term)

-- Var(t)
variables :: Term -> [Var]
variables (V x)    = [x]
variables (F _f ts) = List.nub [ x | ti <- ts, x <- variables ti ]

varsInList :: [Term] -> [Var]
varsInList ts = List.nub [ v | t <- ts, v <- variables t]

varsInPair :: TermPair -> [Var]
varsInPair (t, s) = varsInList [t, s]

varsInES :: [TermPair] -> [Var]
varsInES ps = List.nub [x | p <- ps, x <- varsInPair p]

isGround :: Term -> Bool
isGround t = null (variables t)

isGroundTermPair :: TermPair -> Bool
isGroundTermPair (s, t) = isGround s && isGround t

functions :: Term -> [Function]
functions (V _) = []
functions (F f ts) = List.nub (f : [f' | t <- ts, f' <- functions t])

functionsInPair :: TermPair -> [Function]
functionsInPair (t, s) = List.nub (functions t ++ functions s)

functionsInES :: [TermPair] -> [Function]
functionsInES ps = List.nub [f | p <- ps, f <- functionsInPair p ]

type Subst = [(Var, Term)]

-- substitute t σ= tσ
substitute :: Term -> Subst -> Term
substitute t@(V x) s
  | Just u <- lookup x s = u
  | otherwise = t
substitute (F f ts) s = F f [ substitute t s | t <- ts ]
