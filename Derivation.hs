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

-- Single step of rewriting. (by, from, to)
type TermRewriteStep = (Int, Term, Term)

ruleOfTermRewriteStep :: TermRewriteStep -> Int
ruleOfTermRewriteStep (i, _, _) = i

showTermRewriteStep :: TermPrinter -> TermRewriteStep -> BSB.Builder
showTermRewriteStep pr (i, from, to) = showTerm pr from <> BSB.string7 " => " <> showTerm pr to

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
    showSimpRewriteSteps _tp (simpRewriteSteps d)
showDerivation _tp (Goal {rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite lhs with equations ["  <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | (n, f, t) <- ls ]) <> BSB.string7 "]\n" <>
    showTermRewriteSteps _tp ls <> BSB.string7 "\n" <>
    BSB.string7 "               rhs with equations [" <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | (n, f, t) <- rs ]) <> BSB.string7 "]\n" <>
    showTermRewriteSteps _tp rs <> BSB.string7 ".\n"


showSimpRewriteSteps :: TermPrinter -> [(Int, (Term, Term), (Term, Term))] -> BSB.Builder
showSimpRewriteSteps pr [] = BSB.string7 ""
showSimpRewriteSteps pr ((by, from, to) : steps) =
  BSB.string7 "=> equation " <> BSB.intDec by <> BSB.string7 "\n" <>
  showTerm pr (fst to) <> BSB.string7 " = " <> showTerm pr (snd to) <> BSB.string7 "\n" <>
  showSimpRewriteSteps pr steps


-- convert to rewrite steps for equation (by, from_eq, to_eq)
simpRewriteSteps :: Derivation -> [(Int, (Term, Term), (Term, Term))]
simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = [], rw_l = []}) = []
simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = rs, rw_l = []})
  = rhs_part
  where rhs_part = [ (by, (l, f), (l, t)) | (by, f, t) <- rs ]
simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = [], rw_l = ls})
  = lhs_part
  where lhs_part = [ (by, (f, r), (t, r)) | (by, f, t) <- ls ]
simpRewriteSteps (Simp { orig_eqn = (l, r), rw_r = rs, rw_l = ls})
  = lhs_part ++ rhs_part
  where lhs_part = [ (by, (f, r), (t, r)) | (by, f, t) <- ls ]
        (_, _, last_lhs) = last ls
        rhs_part = [ (by, (last_lhs, f), (last_lhs, t)) | (by, f, t) <- rs ]