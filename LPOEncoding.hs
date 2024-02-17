module LPOEncoding where

import Data.List

import Term hiding (var)
import SMT hiding (variables)
import Util

type LPODic = [((Term, Term), String)]

varF :: Function -> String
varF f = "f_" ++ show f

encodeLPO2aDic :: LPODic -> Term -> Term -> Formula
encodeLPO2aDic dic (F _ ss _) t@(F _ _ _) =
  case [True | si <- ss, si == t] of
    _ : _ -> trueFormula
    [] -> orFormula  [ BoolVar var | si <- ss,
                                     let var = Util.unsafeLookup (si, t) dic ]
encodeLPO2aDic _ _ _ = undefined

encodeLPO2bDic :: LPODic -> Term -> Term -> Formula
encodeLPO2bDic dic s@(F f _ _) (F g ts _)  | f /= g = andFormula (p1 : ps)
  | otherwise = falseFormula
  where p1 = Gt (Var (varF f)) (Var (varF g))
        ps = [ BoolVar var | tj <- ts,
                             let var = Util.unsafeLookup (s, tj) dic]
encodeLPO2bDic _ _ _ = undefined

-- NOTE: LPO2b does not subsume LPO2c.
-- consider f(0, x) >_lpo 0 with empty precedence.
encodeLPO2cDic :: LPODic -> Term -> Term -> Formula
encodeLPO2cDic dic s@(F f ss _) (F g ts _)
  | f == g = andFormula ((lex' ss ts) : ps)
  | otherwise = falseFormula
  where ps = [ BoolVar var | tj <- ts, let var = Util.unsafeLookup (s, tj) dic]
        lex' _ [] = falseFormula 
        lex' [] _ = falseFormula
        lex' (si : ss') (tj : ts')
          | si == tj = lex' ss' ts'
          | otherwise = BoolVar (Util.unsafeLookup (si, tj) dic)
encodeLPO2cDic _ _ _ = undefined

-- cf. term rewriting and all that, Definition 5.4.12
-- encode a rule l -> r into smt-lib constraints for l >_lpo r 
-- encodeLPO l r
encodeLPODic :: LPODic -> Term -> Term -> Formula
encodeLPODic _ (V _ _) _ = falseFormula
encodeLPODic _ s (V x _)
  | x `elem` (variables s) = trueFormula
  | otherwise = falseFormula
encodeLPODic dic s t = orFormula [encodeLPO2aDic dic s t, encodeLPO2bDic dic s t, encodeLPO2cDic dic s t]

constructDic :: [TermPair] -> LPODic
constructDic rules = zip possiblePairs [ "__lpo" ++ show i | i <- ([0..] :: [Int])]
  where possiblePairs = nub [ (s, t) | (l, r) <- rules, s <- subterms l, t <- subterms r]

compareBySMTValue :: (a, SMTValue) -> (a, SMTValue) -> Ordering
compareBySMTValue (_, n1) (_, n2) = compare (smtValueUnsafeCastInt n2) (smtValueUnsafeCastInt n1)

-- findPrecedence :: TRS -> IO (Maybe Precedence)
-- findPrecedence trs = do
--   res <- z3 "toma" z3Input
--   return $ do
--     res' <- res
--     return (assoc2Prec [ (fname, n) | (fname, v) <- res', n <- fromSMTValue2Int v])
--   where
--     lpoDic = constructDic trs
--     functionSymbols = nub [f | (l, r) <- trs, f <- (Term.functions l) ++ (Term.functions r)]
--     precTotality = Distinct [Var (varF f) | f <- functionSymbols]
--     dicCostraints = andFormula [ impliesFormula (BoolVar name) (encodeLPODic lpoDic s t) | ((s, t), name) <- lpoDic]
--     orientation = andFormula [ BoolVar (Util.unsafeLookup rule lpoDic) | rule <- trs]
--     z3Input = sat functionSymbols (map snd lpoDic) (andFormula [orientation, dicCostraints, precTotality])
