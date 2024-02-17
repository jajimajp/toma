module SplitIf (splitIfTPTP, splitIfINF, trueSymbol, falseSymbol, trueTerm, falseTerm) where

import CES
import Data.List
import INFProblem
import ParserTerm as PT
import Text.Read
import TPTP

-- This module implements the split-if transformation for
-- * the infeasibility problem of CoCo, and
-- * first-order Horn theories.

-- Checking if a Horn cluase admits a trivial model.

type HornClause a = ([a], Maybe a)
-- E.g.,
--   ([1,2,3], Just 4)  stands for "(1 && 2 && 3) -> 4".
--   ([1,2,3], Nothing) stands for "(1 && 2 && 3) -> False".

-- Satisfiability checker of propositional Horn formulas

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll x ys = [ y | y <- ys, y /= x ]

satisfiable :: Eq a => [HornClause a] -> Bool
satisfiable cs = satisfiable1 cs cs
  where
    satisfiable1 _cs1 []                   = True
    satisfiable1 _cs1 (([], Nothing) : _cs) = False
    satisfiable1 cs1 (([], y@(Just a)) : _cs) = satisfiable cs2 
      where cs2 = [ (deleteAll a xs, z) | (xs, z) <- cs1, y /= z ]
    satisfiable1 cs1 (_ : cs') = satisfiable1 cs1 cs'

encodeTPTP :: TPTP -> Maybe [HornClause Atom]
encodeTPTP tptp = sequenceA [ encodeClause clause | CNF _ _ clause <- tptp ]

encodeClause :: [(Bool, a)] -> Maybe ([a], Maybe a)
encodeClause []                               = Just ([], Nothing)
encodeClause ((True, z) : clause)
  | Just (xs, Nothing) <- encodeClause clause = Just (xs, Just z)
encodeClause ((False, z) : clause)
  | Just (xs, ys) <- encodeClause clause      = Just (z : xs, ys)
encodeClause _ = Nothing 

propositionalEncoding :: [([Atom], Maybe Atom)] -> [([String], Maybe String)]
propositionalEncoding hornClauses =
  [ ([ f | Predicate (F f _) <- ts ], Nothing)
  | (ts, Nothing) <- hornClauses ] ++
  [ ([ f | Predicate (F f _) <- ts ], Just g)
  | (ts, Just (Predicate (F g _))) <- hornClauses ]

hasTrivialModel :: [HornClause Atom] -> Bool
hasTrivialModel hornClauses = satisfiable (propositionalEncoding hornClauses)

-- Trasformation from Horn clauses to word problems
-- (Claessen and Smallbone, IJCAR 2018)

trueSymbol :: String
trueSymbol  = "true__"

falseSymbol :: String
falseSymbol = "false__"

trueTerm :: Term
trueTerm    = F trueSymbol []

falseTerm :: Term
falseTerm   = F falseSymbol []

liftAtom :: Atom -> (Term, Term)
liftAtom (Predicate t)  = (t, F trueSymbol [])
liftAtom (Equation s t) = (s, t)

liftHornClause :: HornClause Atom -> CEquation
liftHornClause (ps, Nothing) =
  ([ liftAtom p | p <- ps], (F falseSymbol [], F trueSymbol []))
liftHornClause (ps, Just e)  =
  ([ liftAtom p | p <- ps], liftAtom e)

liftHornClauses :: [HornClause Atom] -> CES
liftHornClauses hornClauses = [ liftHornClause c | c <- hornClauses ]

fresh :: Int -> String
fresh n = "f" ++ show n

functionsInCES :: [([PE], PE)] -> [String]
functionsInCES ces = functionsInES [ e' | (es, e) <- ces, e' <- e : es ]

fSuffix :: String -> Maybe Int
fSuffix ('f' : s) = readMaybe s
fSuffix _         = Nothing

fIndex :: [([PE], PE)] -> Int
fIndex ces =
  foldl max 0 [ n | f <- functionsInCES ces, Just n <- [fSuffix f] ] + 1

splitIfCES' :: Int -> CES -> PES
splitIfCES' _n [] = []
splitIfCES' n (([], e) : hornClauses) = e : splitIfCES' n hornClauses
splitIfCES' n (((s0, t0) : es, (u, v)) : hornClauses) =
  (F f (s : ws), u) : splitIfCES' (n + 1) ((es, (F f (t : ws), v)) : hornClauses)
  where
    f = fresh n
    ws = [ V x | x <- nub (PT.variables s ++ PT.variables u ++ PT.variables v) ]
    (s, t) = if length (PT.variables s0) < length (PT.variables t0) then (s0, t0) else (t0, s0)

splitIfCES :: CES -> PES
splitIfCES hornClauses = splitIfCES' (fIndex hornClauses) hornClauses

-- ES: axioms
-- Equation: goal
splitIfTPTP :: TPTP -> Maybe (PES, PE)
splitIfTPTP tptp
  | Just hornClauses <- encodeTPTP tptp =
      if hasTrivialModel hornClauses then
        Just ([], (trueTerm, trueTerm))
      else
        Just (splitIfCES (liftHornClauses hornClauses), (trueTerm, falseTerm))
splitIfTPTP _ = Nothing

-- copy and paste from Moca...

-- encoding for CoCo INF problems
encodeINF :: INFProblem -> [HornClause Atom]
encodeINF (SemiEquational ceses)   = infToHorn ceses
encodeINF (Join ceses)             = infToHorn ceses
encodeINF (Oriented ceses)         = infToHorn ceses

infToHorn :: (CES, PES) -> [HornClause Atom]
infToHorn (ces, es) = [ (toAtoms es', Just (Equation l r)) | (es', (l, r)) <- ces ++ [ceqForCondition es] ] 
                   ++ [([Equation trueTerm falseTerm], Nothing)]
  where ceqForCondition es' = (es', (trueTerm, falseTerm))
        toAtoms es' = [ Equation l r | (l, r) <- es' ]

-- ES: axioms
-- Equation: goal
splitIfINF :: INFProblem -> Maybe (PES, PE)
splitIfINF p =
  let hornClauses = encodeINF p
  in if hasTrivialModel hornClauses
     then Just ([], (trueTerm, trueTerm))
     else Just (splitIfCES (liftHornClauses hornClauses), (trueTerm, falseTerm))
