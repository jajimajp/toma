module PES where

import qualified Data.ByteString.Builder as BSB
import Data.List
import PE
import Term

type PES = [PE]

-- pretty printer.
showPES :: TermPrinter -> PES -> BSB.Builder
showPES p es = mconcat (intersperse (BSB.string8 "\n") [ showPE p e | e <- es ])

-- If you want to distinguish l = r from r = l,
-- you cannot use `union`. Use ++ instead.
union :: PES -> PES -> PES
union es1 es2 = nubBy PE.variant (es1 ++ es2)

concat :: [PES] -> PES
concat []           = []
concat (es1 : rest) = PES.union es1 (PES.concat rest)

elem :: PE -> PES -> Bool
elem e es = any (PE.variant e) es

-- es1 \ es2
diff :: PES -> PES -> PES
diff es1 es2 = [ e | e <- es1, not (PES.elem e es2) ]

-- E^{-1}
inverse :: PES -> PES
inverse es = [ (r, l) | (l, r) <- es]

sortByWeight :: PES -> PES
sortByWeight es = map snd (sortBy (\(d1, _) (d2, _) -> compare d1 d2)
                                  [(sz, e) | e@(s,t) <- es, let sz = Term.size s + Term.size t])

varsInES :: PES -> [Var]
varsInES ps = nub [x | p <- ps, x <- varsInPair p]

functionsInES :: PES -> [Function]
functionsInES ps = nub [f | p <- ps, f <- functionsInPair p]

-- NOTE: this function assumes that arity is consistent.
functionsInESWithArity :: PES -> [(Function,Int)]
functionsInESWithArity ps = nub (concatMap functionsWithArity (ls ++ rs))
  where (ls, rs) = unzip ps

-- not neccesary for now?
-- rename :: Int -> PES -> PES
