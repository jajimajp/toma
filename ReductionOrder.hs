module ReductionOrder where
-- abstraction for reduction orders.
-- especially, this module provides wrappers for functions depending on reduction orders.

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import CP
import ES
import Term
import LPO
import Join
-- import GWPO

data Class = LPO
          --  | GWPO
          --  | GWPON
          --  | GWPO01
          --  | WPO
          --  | LPOviaGWPO -- LPO simulation via GWPO.

data ReductionOrderParam = LPOParam Precedence
-- | GWPOParam Precedence Algebra | WPOParam Precedence Algebra

-- NOTE: moving this to other module leads to orphan instance warning.
-- instance Show ReductionOrderParam where
  -- show (LPOParam prec p) = "LPO with precedence: " ++ (showPrec p prec)
  -- show (GWPOParam prec alg) =
  --   "GWPO: \n"  ++ showWPOParam (alg, prec)
  -- show (WPOParam prec alg) =
  --   "WPO: \n"  ++ showWPOParam (alg, prec)

showReductionOrderParam :: FunctionPrinter -> ReductionOrderParam -> BSB.Builder
showReductionOrderParam p (LPOParam prec) = BSB.string7 "LPO with precedence: " <> (showPrec p prec)

param2ord :: ReductionOrderParam -> TermOrder
param2ord (LPOParam prec) = gtLPO prec
-- param2ord (GWPOParam prec alg) = gtGWPO (alg, prec)
-- param2ord (WPOParam prec alg) = gtWPO (alg, prec)

-- wrapper
groundJoinable :: ES -> ES -> ReductionOrderParam -> Term -> Term -> Bool
groundJoinable oriented unoriented (LPOParam prec) s t = Join.groundJoinableLPO oriented unoriented prec s t
-- groundJoinable p es s t = Join.joinable es (param2ord p) s t

groundConfluent :: ES -> ES -> ReductionOrderParam -> Bool
groundConfluent oriented unoriented p@(LPOParam _) =
  all (\(s, t) -> groundJoinable oriented unoriented p s t) (map fst (cp gt (oriented ++ unoriented)))
  where gt = param2ord p
-- confluence implies ground confluence
-- groundConfluent p es = all (\(s, t) -> s `gt` t || t `gt` s) (toPES es) && -- is it a TRS? 
--   all (\(s, t) -> Join.joinable es gt s t) (map fst (cp gt es)) --  are all critical pairs joinable?
--   where gt = param2ord p
