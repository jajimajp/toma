module CP where
-- critical pair

import Control.Monad
import Data.Heap as H
import Data.Maybe
import Derivation
import E
import ES
import PE
import Substitution
import Term

newtype CPHeapItem = CPHeapItem (PE, Derivation)

type CPHeap = MinHeap CPHeapItem

-- 
instance Eq CPHeapItem where
  (CPHeapItem (e1, _)) == (CPHeapItem (e2, _)) = variant e1 e2

instance Ord CPHeapItem where
  compare (CPHeapItem (e1, _)) (CPHeapItem (e2, _)) = compare (PE.size e1) (PE.size e2)

-- returns Maybe (new rule, superposition)
cp' :: TermOrder -> PE -> Bool -> PE -> Bool -> Position -> Maybe (PE, Term)
cp' gt rule1 o1 rule2 o2 p = do
  theta <- unify (subtermAt l1 p) l2
  let il1 = Substitution.substitute l1 theta
  let ir1 = Substitution.substitute r1 theta
  let il2 = Substitution.substitute l2 theta
  let ir2 = Substitution.substitute r2 theta
  guard (o1 || not (gt ir1 il1))
  guard (o2 || not (gt ir2 il2))
  return ((ir1, replace il1 ir2 p), il1)
  where ((l1,r1), (l2,r2)) = PE.rename (rule1, rule2)

rules :: E -> [PE]
rules e
  | E.oriented e = [ E.unsafeRule e ]
  | E.isCommutative e = [ eqn e ]
  | otherwise = [ eqn e, PE.reverse (eqn e) ]

cp2 :: TermOrder -> ES -> ES -> [(PE, Derivation)]
cp2 gt es1 es2 = do
-- gt is order > on terms
-- p is non-variable position of l2 where (l2, _) = e2
  e1@(E { eqn_id = i1 }) <- es1
  e2@(E { eqn_id = i2 }) <- es2
  e1'@(l1, _) <- rules e1
  e2'         <- rules e2
  p <- functionPositions l1
-- if p is root position, then e1' and e2' are not variants.
  guard (p /= [] || not (PE.variant e1' e2'))
  (e, sp) <- maybeToList (cp' gt e1' (oriented e1) e2' (oriented e2) p)
  return (e, CP i1 i2 sp)

h :: TermOrder -> ES -> CPHeap
h gt es = H.fromList [ CPHeapItem c | c <- cp2 gt es es]

-- CP(E) : (extended critical pairs)
-- NOTE: may contain duplicates, or joinable CPs
cp :: TermOrder -> ES -> [(PE, Derivation)]
cp gt es = [ (e, d) | CPHeapItem (e, d) <- H.toAscList (h gt es) ] 
