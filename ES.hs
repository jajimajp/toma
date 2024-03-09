{-# LANGUAGE NamedFieldPuns #-}
module ES where

import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import Data.List as DL
import Data.Graph
import Derivation
import E
import PE
import PES
import Term


type ES = [E]

showES :: TermPrinter -> ES -> BSB.Builder
showES p es = mconcat (intersperse (BSB.string7 "\n") (map (showE p) es))

showOTRS :: TermPrinter -> TermOrder -> ES -> BSB.Builder
showOTRS _ _ [] = BSB.string7 ""
showOTRS p gt (E { eqn = (s, t), eqn_id } : es)
  | s `gt` t = BSB.string7 (show eqn_id) <> BSB.string7 ": "
               <> showTerm p s <> BSB.string7 " -> " <> showTerm p t <> BSB.string7 "\n" <>
               showOTRS p gt es
  | t `gt` s = BSB.string7 (show eqn_id) <> BSB.string7 ": "
               <> showTerm p t <> BSB.string7 " -> " <> showTerm p s <> BSB.string7 "\n" <>
               showOTRS p gt es
  | otherwise = BSB.string7 (show eqn_id) <> BSB.string7 ": "
               <> showTerm p s <> BSB.string7 " = " <> showTerm p t <> BSB.string7 "\n" <>
               showOTRS p gt es

maxId :: ES -> Int
maxId es = maximum [ n | E {eqn_id = n} <- es ]

nextId :: ES -> Int
nextId es = 1 + maxId es 

-- NOTE: ignores non-active equations
toPES :: ES -> PES
toPES es = [ eqn e | e <- es ]

axiom :: Int -> PE -> E
axiom i pe
  = E { eqn = pe, eqn_id = i, eqn_derivation = Axiom, eqn_orientation = Unoriented }

axioms :: PES -> ES
axioms pes = [ axiom i pe | (i, pe) <- zip [0..] pes ]

-- checks if variant is in the ES.
inES :: PE -> ES -> Bool
inES _ [] = False
inES pe (e : es) =
  variant pe (eqn e) || inES pe es

sortByWeight :: ES -> ES
sortByWeight es = sortBy comp es 
  where comp (E { eqn = e1 }) (E { eqn = e2 })
          = compare (PE.size e1) (PE.size e2)

sortById :: ES -> ES
sortById es = sortBy comp es
  where comp (E {eqn_id = i1}) (E {eqn_id = i2})
          = compare i1 i2

build :: ES -> Graph
build es = buildG (0, m) edges'
  where m = maxId es
        edges' = [ ed | e <- es, ed <- depE e]

-- filter irrelevant axioms and lemmata
relevant :: Int -> ES -> ES
relevant goal_id es = [ e | e@(E {eqn_id = i}) <- es, DL.elem i relevant_ids ]
  where g = build es
        relevant_ids = reachable g goal_id

orient :: TermOrder -> ES -> ES
orient gt es = [ E.orient gt e | e <- es ]

unorient :: ES -> ES
unorient es = [ E.unorient e | e <- es ]

-- separate oriented and unoriented
-- returns (oriented, unoriented)
separate :: TermOrder -> ES -> (ES, ES)
separate gt es = ([ e | e <- es', E.oriented e ], [ e | e <- es', not (E.oriented e) ])
  where
    es' = [ E.orient gt e | e <- es]
