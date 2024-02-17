module Join where
-- ground joinability testing

import Data.List as DL
import ES
import LPO
import Substitution
import Term
import Util
import Rewriting
import CP

type VariableOrder = [Var]

gtELPOlex :: Precedence -> VariableOrder -> Term -> [Term] -> [Term] -> Bool
-- gtELPOlex prec vgt s ss ts
-- assummption length ss = length ts
gtELPOlex _ _ _ [] [] = False
gtELPOlex _ _ _ _ [] = error "length ss = length ts does not hold."
gtELPOlex _ _ _ [] _ = error "length ss = length ts does not hold."
gtELPOlex prec vgt s (si : ss) (ti : ts)
  | si == ti = gtELPOlex prec vgt s ss ts
  | otherwise = gtELPO prec vgt si ti && all (\tj -> gtELPO prec vgt s tj) ts

-- extended lpo >_elpo
-- assume that prec is total precedence.
gtELPO :: Precedence -> VariableOrder -> Term -> Term -> Bool
gtELPO _prec vgt (V x _) (V y _) = Util.gtByList vgt x y
gtELPO _prec _vgt s (V y _) = DL.elem y (variables s)
-- note that the 3 cases below are NOT distinct.
gtELPO prec vgt s@(F f ss _) t@(F g ts _)
  | Term.gtPrec prec f g, all (\ti -> gtELPO prec vgt s ti) ts = True
  | any (\si -> si == t || gtELPO prec vgt si t) ss = True
  | f == g , gtELPOlex prec vgt s ss ts = True
  | otherwise = False
gtELPO _ _ _ _ = False

-- `enumerateVSubs vs` enumerates variable subsitutions on vs.
enumerateVSubs :: [Var] -> [Subst]
-- just lifts to substitutions.
enumerateVSubs vs = [ [(x, var y) | (x, y) <- f] | f <- Util.allFunctions vs vs]

-- returns True if s and t are joinable w.r.t. OTRS (E, >_lpo)
-- Note that False means MAYBE i.e. this function is sound but not complete.
-- very inefficient for now...
groundJoinableLPO :: ES -> ES -> Precedence -> Term -> Term -> Bool
groundJoinableLPO oriented unoriented prec s t =
  joinable oriented unoriented (gtLPO prec) s t || -- if joinable, don't need to use Martin and Nipkow's method.
  all (\(rho, vord) -> joinable [] (ES.unorient oriented ++ unoriented) (gtELPO prec vord) (substitute s rho) (substitute t rho))
      [(rho, vord) | rho <- enumerateVSubs stvars, vord <- allTotalStrictOrders stvars]
  where stvars = DL.nub (variables s ++ variables t)

joinable :: ES -> ES -> TermOrder -> Term -> Term -> Bool
joinable oriented unoriented gt s t = s' == t'
  where (_, s') = nf oriented unoriented gt s
        (_, t') = nf oriented unoriented gt t

-- ground confluence testing for OTRS with LPO
-- Note that False means MAYBE i.e. this function is sound but not complete.
groundConfluentLPO :: ES -> ES -> Precedence -> Bool
groundConfluentLPO oriented unoriented prec =
  all (\(s, t) -> groundJoinableLPO oriented unoriented prec s t)
      (map fst (cp (gtLPO prec) (oriented ++ unoriented)))
