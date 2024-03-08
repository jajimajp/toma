module Derivation where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Data.List
import Term

data Derivation = Axiom
                | CP Int Int Term -- Rule1, Rule2, Superposition
                | Simp { original :: Int, rw_r :: [Int], rw_l :: [Int]  }
                | Goal { rw_r :: [Int], rw_l :: [Int] }


showDerivation :: TermPrinter -> Derivation -> BSB.Builder
showDerivation _tp Axiom = BSB.string7 "Proof: Axiom.\n"
showDerivation tp (CP i1 i2 sp)
  = BSB.string7 "Proof: A critical pair between equations "
    <> BSB.intDec i1 <> BSB.string7 " and " <> BSB.intDec i2
    <> BSB.string7 " with superposition " <> showTerm tp sp
    <> BSB.string7 ".\n"
showDerivation _tp (Simp { original = orig, rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite equation " <> BSB.intDec orig <> BSB.string7 ",\n" <>
    BSB.string7 "               lhs with equations [" <> mconcat (intersperse (BSB.string7 ",") [ BSB.intDec n | n <- ls ]) <> BSB.string7 "]\n" <>
    BSB.string7 "               rhs with equations [" <>  mconcat (intersperse (BSB.string7 ",") [ BSB.intDec n | n <- rs ]) <> BSB.string7 "].\n"
showDerivation _tp (Goal {rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite lhs with equations ["  <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | n <- ls ]) <> BSB.string7 "]\n" <>
    BSB.string7 "               rhs with equations [" <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | n <- rs ]) <> BSB.string7 "].\n"
