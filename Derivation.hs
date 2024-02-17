module Derivation where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Data.List

data Derivation = Axiom
                | CP Int Int
                | Simp { original :: Int, rw_r :: [Int], rw_l :: [Int]  }
                | Goal { rw_r :: [Int], rw_l :: [Int] }


showDerivation :: Derivation -> BSB.Builder
showDerivation Axiom = BSB.string7 "Proof: Axiom.\n"
showDerivation (CP i1 i2)
  = BSB.string7 "Proof: A critical pair between equations " <> BSB.intDec i1 <> BSB.string7 " and " <> BSB.intDec i2 <> BSB.string7 ".\n"
showDerivation (Simp { original = orig, rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite equation " <> BSB.intDec orig <> BSB.string7 ",\n" <>
    BSB.string7 "               lhs with equations [" <> mconcat (intersperse (BSB.string7 ",") [ BSB.intDec n | n <- ls ]) <> BSB.string7 "]\n" <>
    BSB.string7 "               rhs with equations [" <>  mconcat (intersperse (BSB.string7 ",") [ BSB.intDec n | n <- rs ]) <> BSB.string7 "].\n"
showDerivation (Goal {rw_r = rs, rw_l = ls})
  = BSB.string7 "Proof: Rewrite lhs with equations ["  <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | n <- ls ]) <> BSB.string7 "]\n" <>
    BSB.string7 "               rhs with equations [" <> mconcat (intersperse (BSB.string7 ",")  [ BSB.intDec n | n <- rs ]) <> BSB.string7 "].\n"
