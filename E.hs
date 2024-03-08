{-# LANGUAGE NamedFieldPuns #-}
module E where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Derivation
import PE
import Term

data Orientation = LR -- l -> r
                 | RL -- r -> l
                 | Unoriented 

-- do not swap lhs and rhs of eqn
-- because it can lead inconsistency with derivation.
data E = E { eqn :: PE,
             eqn_id :: Int,
             eqn_orientation :: Orientation,
             eqn_derivation :: Derivation }

-- convert to rewrite rule
rule :: E -> Maybe TermPair
rule (E { eqn_orientation = LR, eqn = (l, r) }) = Just (l, r)
rule (E { eqn_orientation = RL, eqn = (l, r) }) = Just (r, l)
rule (E { eqn_orientation = Unoriented }) = Nothing

unsafeRule :: E -> TermPair
unsafeRule e
  | Just (l, r) <- rule e = (l, r)
  | otherwise = error "unsafeRule: equation is not oriented."

oriented :: E -> Bool
oriented (E { eqn_orientation = Unoriented }) = False
oriented _ = True

orient :: TermOrder -> E -> E
orient gt (E { eqn = (l, r), eqn_id, eqn_derivation })
  | l `gt` r = E { eqn = (l, r), eqn_id = eqn_id, eqn_orientation = LR, eqn_derivation = eqn_derivation }
  | r `gt` l = E { eqn = (l, r), eqn_id = eqn_id, eqn_orientation = RL, eqn_derivation = eqn_derivation }
  | otherwise = E { eqn = (l, r), eqn_id = eqn_id, eqn_orientation = Unoriented, eqn_derivation = eqn_derivation }

unorient :: E -> E
unorient (E { eqn, eqn_id, eqn_derivation })
  = (E { eqn = eqn, eqn_id = eqn_id, eqn_orientation = Unoriented, eqn_derivation = eqn_derivation })

showE :: TermPrinter -> E -> BSB.Builder
showE p (E { eqn = (l, r), eqn_id = i, eqn_derivation = d  })
  = (BSB.intDec i) <> (BSB.string7 ": ") <> showTerm p l <> (BSB.string7 " = ") <> showTerm p r <> (BSB.string7 ".\n") <>
    showDerivation p d

-- dependency of derivation
depE :: E -> [(Int, Int)]
depE (E {eqn_derivation = Axiom}) = []
depE (E {eqn_derivation = CP i1 i2 _sp, eqn_id = i })
  = [(i, i1), (i, i2)]
depE (E {eqn_derivation = Simp { original, rw_r, rw_l}, eqn_id = i})
  = (i, original) : [ (i, i') | i' <- rw_r ] ++ [ (i, i') | i' <- rw_l ]
depE (E {eqn_derivation = Goal { rw_r, rw_l }, eqn_id = i})
  = [ (i, i') | i' <- rw_r ] ++ [ (i, i') | i' <- rw_l ]

isCommutative :: E -> Bool
isCommutative = PE.isCommutative . eqn
