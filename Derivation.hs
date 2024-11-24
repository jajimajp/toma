module Derivation where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Data.List
import Term
import PE

data Derivation = Axiom
                | CP Int Int Term -- Rule1, Rule2, Superposition
                | Simp { original :: Int, orig_eqn :: PE, rw_r :: [TermRewriteStep], rw_l :: [TermRewriteStep]  }
                | Goal { rw_r :: [TermRewriteStep], rw_l :: [TermRewriteStep] }

-- Single step of rewriting. (by, pos)
type TermRewriteStep = (Int, Position)

showPosition :: Position -> BSB.Builder
showPosition [] = BSB.string7 "[]"
showPosition l =
  BSB.string7 "[" <>
    mconcat (intersperse (BSB.string7 ",") [ BSB.intDec n | n <- l ]) <>
  BSB.string7 "]"

ruleOfTermRewriteStep :: TermRewriteStep -> Int
ruleOfTermRewriteStep (i, _) = i

showTermRewriteStep :: TermPrinter -> TermRewriteStep -> BSB.Builder
showTermRewriteStep pr (i, pos) = BSB.string7 "rule " <> BSB.intDec i <>
    BSB.string7 " at " <> showPosition pos

showTermRewriteSteps :: TermPrinter -> [TermRewriteStep] -> BSB.Builder
showTermRewriteSteps pr [] = BSB.string7 ""
showTermRewriteSteps pr [step] = showTermRewriteStep pr step
showTermRewriteSteps pr (step : steps) = showTermRewriteStep pr step <> BSB.string7 ",\n" <> showTermRewriteSteps pr steps


showDerivation :: TermPrinter -> Derivation -> BSB.Builder
showDerivation _tp Axiom = BSB.string7 "Proof: Axiom.\n"
showDerivation tp (CP i1 i2 sp)
  = BSB.string7 "Proof: A critical pair between equations "
    <> BSB.intDec i1 <> BSB.string7 " and " <> BSB.intDec i2
    <> BSB.string7 " with superposition " <> showTerm tp sp
    <> BSB.string7 ".\n"
showDerivation _tp d@(Simp { original = orig, rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite equation " <> BSB.intDec orig <> BSB.string7 ",\n" <>
    showSimpRewriteSteps _tp d
showDerivation _tp (Goal {rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite lhs with equations ["  <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | (n, pos) <- ls ]) <> BSB.string7 "]\n" <>
    showTermRewriteSteps _tp ls <> BSB.string7 "\n" <>
    BSB.string7 "               rhs with equations [" <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | (n, pos) <- rs ]) <> BSB.string7 "]\n" <>
    showTermRewriteSteps _tp rs <> BSB.string7 ".\n"


showSimpRewriteSteps :: TermPrinter -> Derivation -> BSB.Builder
showSimpRewriteSteps pr (Simp { rw_r = [], rw_l = [] }) = BSB.string7 ""
showSimpRewriteSteps pr d@(Simp { rw_l = (by, pos) : rw_l }) =
  BSB.string7 "=> equation " <> BSB.intDec by <> BSB.string7 ", lhs at " <>
    showPosition pos <> BSB.string7 "\n" <>
  showSimpRewriteSteps pr d { rw_l = rw_l }
showSimpRewriteSteps pr d@(Simp { rw_r = (by, pos) : rw_r }) =
  BSB.string7 "=> equation " <> BSB.intDec by <> BSB.string7 ", rhs at " <>
    showPosition pos <> BSB.string7 "\n" <>
  showSimpRewriteSteps pr d { rw_r = rw_r }


-- convert to rewrite steps for equation (by, from_eq, to_eq)
-- simpRewriteSteps :: Derivation -> [(Int, (Term, Term), (Term, Term))]
-- simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = [], rw_l = []}) = []
-- simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = rs, rw_l = []})
--   = rhs_part
--   where rhs_part = [ (by, (l, f), (l, t)) | (by, pos) <- rs ]
-- simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = [], rw_l = ls})
--   = lhs_part
--   where lhs_part = [ (by, (f, r), (t, r)) | (by, f, t) <- ls ]
-- simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = rs, rw_l = ls})
--   = lhs_part ++ rhs_part
--   where lhs_part = [ (by, (f, r), (t, r)) | (by, f, t) <- ls ]
--         (_, _, last_lhs) = last ls
--         rhs_part = [ (by, (last_lhs, f), (last_lhs, t)) | (by, f, t) <- rs ]