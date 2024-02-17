module PE where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Term
import Substitution

type PE = TermPair -- primitive equation

showPE :: TermPrinter -> PE -> BSB.Builder
showPE p (s, t) = showTerm p s <> BSB.string7 " = " <> showTerm p t

-- variant as (directed) rules
-- TODO: is this correct?
-- l1 -> r1 and l2 -> r2 are the same rules
-- if (l1 -> r1) \preceq (l2 -> r2) and (l2 -> r2) \preceq (l1 -> r1)
-- where (l1 -> r1) \preceq (l2 -> r2)
--       if there exists substitution σ such that l1 σ = l2 and r1 σ = r2
variantAsRule :: PE -> PE -> Bool
variantAsRule rule1 rule2 = PE.size rule1 == PE.size rule2 -- optimization by sizes as hash
  && compare' rule1 rule2 && compare' rule2 rule1
  where compare' (l1, r1) (l2, r2) =
          case match l1 l2 of -- NOTE: we can't use unify. consider f(x,x) -> x and f(x,y) -> x.
            Nothing -> False
            Just sigma -> Substitution.substitute r1 sigma == r2

-- variant as (undirected) equations
-- NOTE: Equation.variant (l, r) (r,l) = True
variant :: PE -> PE -> Bool
variant (s1, t1) (s2, t2) =
  variantAsRule (s1, t1) (s2, t2) || variantAsRule (s1, t1) (t2, s2) ||
  variantAsRule (t1, s1) (s2, t2) || variantAsRule (t1, s1) (t2, s2) 

-- rename :: PE -> Int -> (Int, PE)
-- rename (l,r) n = (n + length vars, (substitute l sigma, substitute r sigma))
--   where
--     vars = nub (variables l ++ variables r)
--     sigma = zip vars [ V (v ++ "_" ++ show i) | (v, i) <- zip vars [n..] ]

variables :: PE -> [Var]
variables (l, r) = varsInList [l, r]

substitute :: PE -> Subst -> PE
substitute (l, r) s = (Substitution.substitute l s, Substitution.substitute r s)

rename :: (PE, PE) -> (PE, PE)
rename (e1@(l1, r1), e2@(l2, r2)) = (e1, PE.substitute e2 sigma)
  where
    fvs = freshVariables [l1, r1, l2, r2]
    vs2 = varsInList [l2, r2]
    sigma = zip vs2 [ var v | v <- fvs ]

reverse :: PE -> PE
reverse (l, r) = (r, l)

size :: PE -> Int
size (s, t) = Term.size s + Term.size t

isGround :: PE -> Bool
isGround (s, t) = Term.isGround s && Term.isGround t

isCommutative :: PE -> Bool
isCommutative (F f1 [V x1 _, V y1 _] _, F f2 [V y2 _, V x2 _] _)
  = f1 == f2 && x1 == x2 && y1 == y2
isCommutative _ = False
