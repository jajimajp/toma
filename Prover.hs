{-# LANGUAGE NamedFieldPuns #-}
module Prover where

import Data.List
import qualified E as E
import ES
import Derivation
import PE
import PES
import SMT
import Z3
import Term
import LPOEncoding
import Rewriting
import CP
import KBCompletion
import Substitution
import ReductionOrder
import Util
-- import GWPO
import Join

import Control.Monad
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import System.IO

-- NOTE: The prefix "proof_" is to avoid shadow-binding warnings.
data Proof = Join { proof_goal :: E.E, proof_es :: ES, proof_deleted_es :: ES }
           -- system is ground-complete but goal is not joinable.
           | Complete { proof_es :: ES, proof_reduction_order_param :: ReductionOrderParam , proof_deleted_es :: ES}
           | Failure -- not used for now

findLPOPrec :: PES -> PE -> IO Precedence
findLPOPrec es goal = do
  res <- z3 "toma" z3Input
  case res of
    Nothing -> error "findLPOPrec: no precedence found." -- this error means bug?
    Just res' -> return (assoc2Prec [ (f, evalModel (varF f) res') | f <- functionSymbols])
  where
    esWithIds = [("__rulevar" ++ show i, e) | (i, e) <- zip ([0..] :: [Int]) (es ++ PES.inverse es)]
    -- NOTE: include goal (reduction orders require total precedence)
    functionSymbols = nub [ f | (l, r) <- goal : es, f <- functionsInPair (l, r)]
    esIds = [ esId | (esId, _) <- esWithIds ]
    reducibilityOnTerm t =
      orFormula [ BoolVar ruleId | (ruleId, (l, _)) <- esWithIds, encompass t l ]
    test_terms = nub [ u | (s, t) <- goal : es, u <- [s, t] ]
    maximizedConstraints = [ reducibilityOnTerm t | t <- test_terms ]
    lpoDic = constructDic (map snd esWithIds)
    dicCostraints = andFormula [ impliesFormula (BoolVar name) (encodeLPODic lpoDic s t) | ((s, t), name) <- lpoDic]
    orientation = andFormula [ impliesFormula (BoolVar ruleId) (BoolVar (Util.unsafeLookup (l, r) lpoDic))
                             | (ruleId, (l, r)) <- esWithIds ]
    -- totality is required since precedence is presented as list
    precTotality = Distinct [Var (varF f) | f <- functionSymbols]
    z3Input = maxsat [ varF f | f <- functionSymbols]
                     (esIds ++ map snd lpoDic)
                     maximizedConstraints
                     (andFormula [orientation, precTotality, dicCostraints])

-- findGWPOParamN :: PES -> PE -> IO WPOParam
-- findGWPOParamN es goal = do
--   res <- z3 "toma" z3Input
--   case res of
--     Nothing -> error "findGWPOParamN: no parameter is found." -- this error means bug?
--     Just res' ->
--       let prec = assoc2Prec [ (f, n) | f <- z3Vars_prec, SMTIntValue n <- [unsafeLookup f res'] ]
--           alg = [ (c, smtValueUnsafeCastInt (unsafeLookup c res')) | c <- z3Vars_coef ]
--       in return (alg, prec)
--   where
--     esWithIds = [("__rulevar" ++ show i, e) | (i, e) <- zip ([0..] :: [Int]) (es ++ PES.inverse es)]
--     -- NOTE: include goal (reduction orders require total precedence)
--     fs = functionsInESWithArity (goal : es)
--     reducibilityOnTerm t =
--       orFormula [ BoolVar ruleId | (ruleId, (l, _)) <- esWithIds, encompass t l ]
--     test_terms = nub [ u | (s, t) <- goal : es, u <- [s, t] ]
--     maximizedConstraints = [ reducibilityOnTerm t | t <- test_terms ]
--     -- constraits to be satisfied
--     orientation = andFormula [ impliesFormula (BoolVar ruleId) (encodeGWPO N l r)
--                              | (ruleId, (l, r)) <- esWithIds ]
--     precTotality = Distinct [Var f | f <- z3Vars_prec ] -- totality is required since precedence is presented as list
--     coef_nonneg = andFormula [ Geq (Var v) (Val 0) | v <- z3Vars_coef ] -- weak monotonicity
--     -- variables
--     z3Vars_coef = [ ith_coef f i | (f, n) <- fs, i <- [0..n] ]
--     z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
--     esIds = [ esId | (esId, _) <- esWithIds ]
--     z3Input = maxsat (z3Vars_prec ++ z3Vars_coef)
--                      esIds
--                      maximizedConstraints
--                      (andFormula [orientation, precTotality, coef_nonneg])

-- findGWPOParam01 :: PES -> PE -> IO WPOParam
-- findGWPOParam01 es goal = do
--   res <- z3 "toma" z3Input
--   case res of
--     Nothing -> error "findGWPOParam01: no parameter is found." -- this error means bug?
--     Just res' ->
--       let prec = assoc2Prec [ (f, n) | f <- z3Vars_prec, SMTIntValue n <- [unsafeLookup f res'] ]
--           alg = [ (c, smtValueUnsafeCastInt (unsafeLookup c res')) | c <- z3Vars_coef1n ++ z3Vars_coef0 ]
--       in return (alg, prec)
--   where
--     esWithIds = [("__rulevar" ++ show i, e) | (i, e) <- zip ([0..] :: [Int]) (es ++ PES.inverse es)]
--     -- NOTE: include goal (reduction orders require total precedence)
--     fs = functionsInESWithArity (goal : es)
--     reducibilityOnTerm t =
--       orFormula [ BoolVar ruleId | (ruleId, (l, _)) <- esWithIds, encompass t l ]
--     test_terms = nub [ u | (s, t) <- goal : es, u <- [s, t] ]
--     maximizedConstraints = [ reducibilityOnTerm t | t <- test_terms ]
--     -- constraits to be satisfied
--     orientation = andFormula [ impliesFormula (BoolVar ruleId) (encodeGWPO ZeroOne l r)
--                              | (ruleId, (l, r)) <- esWithIds ]
--     precTotality = Distinct [Var f | f <- z3Vars_prec ] -- totality is required since precedence is presented as list
--     coef0_nonneg = andFormula [ Geq (Var v) (Val 0) | v <- z3Vars_coef0 ] -- this may not be neccesary.
--     -- variables
--     z3Vars_coef1n = [ ith_coef f i | (f, n) <- fs, i <- [1..n] ]
--     z3Vars_coef0 = [ ith_coef f 0 | (f, _n) <- fs ]
--     z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
--     esIds = [ esId | (esId, _) <- esWithIds ]
--     z3Input = maxsat (z3Vars_prec ++ z3Vars_coef0)
--                      (esIds ++ z3Vars_coef1n)
--                      maximizedConstraints
--                      (andFormula [orientation, precTotality, coef0_nonneg])


-- findWPOParam :: PES -> PE -> IO WPOParam
-- findWPOParam es goal = do
--   res <- z3 "toma" z3Input
--   case res of
--     Nothing -> error "findWPOParam: no parameter is found." -- this error means bug?
--     Just res' ->
--       let prec = assoc2Prec [ (f, n) | f <- z3Vars_prec, SMTIntValue n <- [unsafeLookup f res'] ]
--           alg = [ (c, smtValueUnsafeCastInt (unsafeLookup c res')) | c <- z3Vars_coef ]
--       in return (alg, prec)
--   where
--     esWithIds = [("__rulevar" ++ show i, e) | (i, e) <- zip ([0..] :: [Int]) (es ++ PES.inverse es)]
--     -- NOTE: include goal (reduction orders require total precedence)
--     fs = functionsInESWithArity (goal : es)
--     reducibilityOnTerm t =
--       orFormula [ BoolVar ruleId | (ruleId, (l, _)) <- esWithIds, encompass t l ]
--     test_terms = nub [ u | (s, t) <- goal : es, u <- [s, t] ]
--     maximizedConstraints = [ reducibilityOnTerm t | t <- test_terms ]
--     -- constraits to be satisfied
--     orientation = andFormula [ impliesFormula (BoolVar ruleId) (encodeWPO N l r)
--                              | (ruleId, (l, r)) <- esWithIds ]
--     precTotality = Distinct [Var f | f <- z3Vars_prec ] -- totality is required since precedence is presented as list
--     coef1n_nonneg = andFormula [ Gt (Var v) (Val 0) | v <- z3Vars_coef1n ] -- strict monotonicity
--     coef0_nat = andFormula [ Geq (Var v) (Val 0) | v <- z3Vars_coef0 ]
--     -- variables
--     z3Vars_coef1n = [ ith_coef f i | (f, n) <- fs, i <- [1..n] ]
--     z3Vars_coef0 = [ ith_coef f 0 | (f, _n) <- fs ]
--     z3Vars_coef = z3Vars_coef0 ++ z3Vars_coef1n
--     z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
--     esIds = [ esId | (esId, _) <- esWithIds ]
--     z3Input = maxsat (z3Vars_prec ++ z3Vars_coef)
--                      esIds
--                      maximizedConstraints
--                      (andFormula [orientation, precTotality, coef1n_nonneg, coef0_nat])

-- findLPOParamViaGWPO :: PES -> PE -> IO Precedence
-- findLPOParamViaGWPO es goal = do
--   res <- z3 "toma" z3Input
--   case res of
--     Nothing -> error "findLPOParamViaGWPO: no parameter is found." -- this error means bug?
--     Just res' ->
--       let prec = assoc2Prec [ (f, n) | f <- z3Vars_prec, SMTIntValue n <- [unsafeLookup f res'] ]
--       in return prec
--   where
--     esWithIds = [("__rulevar" ++ show i, e) | (i, e) <- zip ([0..] :: [Int]) (es ++ PES.inverse es)]
--     -- NOTE: include goal (reduction orders require total precedence)
--     fs = functionsInESWithArity (goal : es)
--     reducibilityOnTerm t =
--       orFormula [ BoolVar ruleId | (ruleId, (l, _)) <- esWithIds, encompass t l ]
--     test_terms = nub [ u | (s, t) <- goal : es, u <- [s, t] ]
--     maximizedConstraints = [ reducibilityOnTerm t | t <- test_terms ]
--     -- constraits to be satisfied
--     orientation = andFormula [ impliesFormula (BoolVar ruleId) (encodeWPO Max l r)
--                              | (ruleId, (l, r)) <- esWithIds ]
--     precTotality = Distinct [Var f | f <- z3Vars_prec ] -- totality is required since precedence is presented as list
--     -- variables
--     z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
--     esIds = [ esId | (esId, _) <- esWithIds ]
--     z3Input = maxsat z3Vars_prec
--                      esIds
--                      maximizedConstraints
--                      (andFormula [orientation, precTotality])


-- NOTE: maximize the # of reducibility.
-- More precisely, maximizes the # of terms t
-- that satisfies the follwoing condition:
-- there exists l = r \in E \cup E^{-1} such that
-- s encompasses l and l >_lpo r.

-- NOTE:
-- Maximizing naively the # of terms t such that t is reducible in (E, >_lpo)
-- seems to generate too many smt variables (?)
-- Encoding itself was also too expensive (?)
-- As a result, the overall procedure became too slow (?)
-- I haven't investigated in detail though...

-- criterion to skip order generation
useSameOrder :: ProverArgs -> Bool
useSameOrder (ProverArgs { conf, iteration_n })
  | Just n <- n_skip_order_search conf = iteration_n > n
  | otherwise = False

findReductionOrderParam :: ProverArgs -> IO ReductionOrderParam
findReductionOrderParam args@(ProverArgs { conf, previousOrder })
  | useSameOrder args, Just p <- previousOrder = do
      when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7 "skip finding a new reduction order (the previous one is used)\n"))
      return p
findReductionOrderParam (ProverArgs { conf, currentES, goal }) =
  case (order_class conf) of
    LPO -> do prec <- findLPOPrec (toPES currentES) goal
              return (LPOParam prec)
    -- GWPO -> do (alg, prec) <- Prover.findGWPOParam01 es goal
    --            return (GWPOParam prec alg)
    -- GWPO01 -> do (alg, prec) <- Prover.findGWPOParam01 es goal
    --              return (GWPOParam prec alg)
    -- GWPON -> do (alg, prec) <- Prover.findGWPOParamN es goal
    --             return (GWPOParam prec alg)
    -- WPO -> do (alg, prec) <- Prover.findWPOParam es goal
    --           return (WPOParam prec alg)
    -- LPOviaGWPO -> do prec <- Prover.findLPOParamViaGWPO es goal
    --                  return (LPOParam prec)

addLemmata' :: Int -> Int -> [(PE, Derivation)] -> ES -> (Int, ES)
addLemmata' next_id _n [] es = (next_id, es)
addLemmata' next_id 0 _ es = (next_id, es)
addLemmata' next_id n (((s, t), d) : cs) es
  -- NOTE: duplicate check is essential
  | not (inES (s, t) es) = addLemmata' (next_id + 1) (n-1) cs (e : es)
  | otherwise = addLemmata' next_id n cs es
  where
    e =  E.E { E.eqn = (s, t), E.eqn_id = next_id, E.eqn_derivation = d, E.eqn_orientation = E.Unoriented }

addLemmata :: Int -> ES -> ES -> ReductionOrderParam -> (Int, ES)
addLemmata next_id oriented unoriented param = addLemmata' next_id n cps (oriented ++ unoriented)
  where cps = [ c | c@((s, t), _d) <- cp (param2ord param) (oriented ++ unoriented),
                    not (Join.joinable oriented unoriented (param2ord param) s t) ]
        n = 12 -- number to be added

prove' :: ProverArgs -> IO Proof
prove' args@(ProverArgs { conf, iteration_n, next_id, goal = goal@(s, t), initES, currentES, deleted  }) = do
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.intDec iteration_n <> BSB.string7 "th iteration...\n"))
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7 "finding a reduction order...\n"))
  order_param <- findReductionOrderParam args
  when (verbose conf)
       (BSB.hPutBuilder stdout (BSB.string7 "found reduction order: " <> (showReductionOrderParam (functionPrinter (termPrinter conf)) order_param) <> BSB.string7 "\n"))
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7 "orienting equations...\n"))
  let gt = param2ord order_param
  let (oriented, unoriented) = ES.separate gt currentES
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7 "performing inter-reduction...\n"))
  let (next_id', oriented', unoriented', deleted') = if inter_reduction conf
                                                    then interreduce next_id oriented unoriented order_param 
                                                    else (next_id, oriented, unoriented, [])
  when (verbose conf)
       (BSB.hPutBuilder stdout (BSB.string7 "# of equations: " <> BSB.intDec (length currentES) <> BSB.string7 " -> " <> BSB.intDec (length (oriented' ++ unoriented')) <> BSB.string7 "\n"))
  let next_deleted = deleted' ++ deleted
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7 "rewriting the goal...\n"))
  let (rw_s, nf_s) = nf oriented' unoriented' gt s
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7 "the lhs " <> showTerm (termPrinter conf) s <> BSB.string7 " is rewritten to " <> showTerm (termPrinter conf) nf_s <> BSB.string7 "\n"))
  let (rw_t, nf_t) = nf oriented' unoriented' gt t
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7  "the rhs " <> showTerm (termPrinter conf) t <> BSB.string7 " is rewritten to " <> showTerm (termPrinter conf) nf_t <> BSB.string7 "\n"))
  when (verbose conf) (BSB.hPutBuilder stdout (BSB.string7 "generating lemmata...\n"))
  let (next_id'', nextES) = addLemmata next_id' oriented' unoriented' order_param
  -- First, the prover checks if s and t are joinable.
  if nf_s == nf_t
    then let
           d = Goal { rw_l = rw_s, rw_r = rw_t }
           g' = E.E { E.eqn = goal, E.eqn_id = next_id', E.eqn_derivation = d, E.eqn_orientation = E.Unoriented }
          in return (Join { proof_goal = g', proof_es = oriented' ++ unoriented', proof_deleted_es =  next_deleted })
    else if groundCompletion conf &&
            all (\(s'', t'') -> ReductionOrder.groundJoinable oriented' unoriented' order_param s'' t'') initES && -- ground equivalence and
            ReductionOrder.groundConfluent oriented' unoriented' order_param -- ground confluence
          then return (Complete { proof_es = oriented' ++ unoriented', proof_reduction_order_param = order_param, proof_deleted_es = next_deleted })
          else prove' (ProverArgs { conf = conf,
                                    iteration_n = iteration_n + 1,
                                    next_id = next_id'',
                                    goal = goal,
                                    initES = initES,
                                    currentES = ES.unorient nextES,
                                    deleted = next_deleted,
                                    previousOrder = Just order_param })

data Config =
  Config { inter_reduction :: Bool,
           order_class :: ReductionOrder.Class,
           verbose :: Bool,
           termPrinter :: TermPrinter,
           groundCompletion :: Bool,
           -- after n iteration, order search is skipped.
           n_skip_order_search :: Maybe Int }

data ProverArgs =
  ProverArgs { conf :: Config, -- does not change throughtout the proving procedure
               iteration_n :: Int,
               next_id :: Int,
               goal :: PE, -- does not change throughtout the proving procedure
               initES :: PES,
               currentES :: ES, -- current equation pool
               deleted :: ES,
               previousOrder :: Maybe ReductionOrderParam }

-- goal must be ground 
prove :: Config -> PE -> ES -> IO Proof
prove conf goal es =
  prove' (ProverArgs { conf = conf,
                       iteration_n = 1,
                       next_id = 1 + maxId es,
                       goal = goal,
                       initES = toPES es,
                       currentES = es,
                       deleted = [],
                       previousOrder = Nothing })
