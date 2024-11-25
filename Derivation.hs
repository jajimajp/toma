module Derivation where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Data.List
import Term
import PE
import qualified Text.Parsec as BSB

data Derivation = Axiom
                | CP Int Int Term -- Rule1, Rule2, Superposition
                | Simp { original :: Int, orig_eqn :: PE, rw_r :: [TermRewriteStep], rw_l :: [TermRewriteStep]  }
                | Goal { rw_r :: [TermRewriteStep], rw_l :: [TermRewriteStep] }

-- Single step of rewriting. (ruleid, pos, direction)
type TermRewriteStep = (Int, Position, Direction)
data Direction = L2R | R2L deriving (Eq)

showPosition :: Position -> BSB.Builder
showPosition [] = BSB.string7 "[]"
showPosition l =
  BSB.string7 "[" <>
    mconcat (intersperse (BSB.string7 ",") [ BSB.intDec n | n <- l ]) <>
  BSB.string7 "]"

ruleOfTermRewriteStep :: TermRewriteStep -> Int
ruleOfTermRewriteStep (i, _, _) = i

showTermRewriteStep :: TermPrinter -> TermRewriteStep -> BSB.Builder
showTermRewriteStep pr (i, pos, dir) =
  BSB.string7 "- " <> BSB.intDec i <>
    BSB.string7 "-" <> (if dir == L2R then BSB.string7 "L->R" else BSB.string7 "R->L") <>
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
showDerivation _tp d@(Goal {rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite goal,\n" <>
    showGoalRewriteSteps _tp d


showSimpRewriteSteps :: TermPrinter -> Derivation -> BSB.Builder
showSimpRewriteSteps pr (Simp { rw_r = [], rw_l = [] }) = BSB.string7 ""
showSimpRewriteSteps pr d@(Simp { rw_l = (by, pos, dir) : rw_l }) =
  BSB.string7 "- lhs by equation " <> BSB.intDec by <>
    BSB.string7 " " <> (if dir == L2R then BSB.string7 "L->R" else BSB.string7 "R->L") <>
    BSB.string7 " at " <> showPosition pos <> BSB.string7 "\n" <>
  showSimpRewriteSteps pr d { rw_l = rw_l }
showSimpRewriteSteps pr d@(Simp { rw_r = (by, pos, dir) : rw_r }) =
  BSB.string7 "- rhs by equation " <> BSB.intDec by <>
    BSB.string7 " " <> (if dir == L2R then BSB.string7 "L->R" else BSB.string7 "R->L") <>
    BSB.string7 " at " <> showPosition pos <> BSB.string7 "\n" <>
  showSimpRewriteSteps pr d { rw_r = rw_r }

showGoalRewriteSteps :: TermPrinter -> Derivation -> BSB.Builder
showGoalRewriteSteps pr (Goal { rw_r = [], rw_l = [] }) = BSB.string7 ""
showGoalRewriteSteps pr d@(Goal { rw_l = (by, pos, dir) : rw_l }) =
  BSB.string7 "- lhs by equation " <> BSB.intDec by <>
    BSB.string7 " " <> (if dir == L2R then BSB.string7 "L->R" else BSB.string7 "R->L") <>
    BSB.string7 " at " <> showPosition pos <> BSB.string7 "\n" <>
  showGoalRewriteSteps pr d { rw_l = rw_l }
showGoalRewriteSteps pr d@(Goal { rw_r = (by, pos, dir) : rw_r }) =
  BSB.string7 "- rhs by equation " <> BSB.intDec by <>
    BSB.string7 " " <> (if dir == L2R then BSB.string7 "L->R" else BSB.string7 "R->L") <>
    BSB.string7 " at " <> showPosition pos <> BSB.string7 "\n" <>
  showGoalRewriteSteps pr d { rw_r = rw_r }