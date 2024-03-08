{-# LANGUAGE NamedFieldPuns #-}
import Data.List as List
import E
import ES hiding (separate)
import Term
import PE
import PES
import UEQParser
import INFParser
import TRSParser
import SplitIf
import Prover
import Util
import ReductionOrder
-- import Termination
import Text.Printf
import System.Environment
import ParserTerm as PT
import Data.Map as Map
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import System.IO

-- INF: CoCo infeasibility problem
-- Termination: termination checker
data Mode = UEQ String
          | TPTP String
          | INF String
          | Help
          | Waldmeister String
          | OrderedCompletion String
          | Termination String (ReductionOrder.Class)
          | CompletionWithParsableOutput String

-- HACK: stub
emptyTermPrinter :: TermPrinter
emptyTermPrinter = (Map.empty, BSB.string7 "BUG") -- never used

-- NOTE: default order should be GWPO01?
defaultConfig :: Prover.Config
defaultConfig =
  Prover.Config { inter_reduction = True,
                  order_class = ReductionOrder.LPO,
                  verbose = False,
                  termPrinter = emptyTermPrinter,
                  groundCompletion = True,
                  n_skip_order_search = Nothing  }

-- config for CASC UEQ
cascConfig :: Prover.Config
cascConfig = 
  Prover.Config { inter_reduction = True,
                  order_class = ReductionOrder.LPO,
                  verbose = False,
                  termPrinter = emptyTermPrinter,
                  groundCompletion = False,
                  n_skip_order_search = Just 8 }

-- convertEquation :: PT.TermPair -> TPTPInput
-- convertEquation (l, r) = TPTP.CNF "some_equation" "axiom" [(True, Equation l r)]

-- convertGoal :: PT.TermPair -> TPTPInput
-- convertGoal (l, r) = TPTP.CNF "some_equation" "negated_conjecture" [(False, Equation l r)]

-- convertES :: [PT.TermPair] -> TPTP
-- convertES es = [ convertEquation e | e <- es ]

-- TODO: hacky implementation
parseArgs' :: [String] -> Mode -> Prover.Config -> (Mode, Prover.Config)
parseArgs' [] m c = (m, c)
-- help
parseArgs' ("-h" : _) _ _ =  (Help, defaultConfig)
parseArgs' ("--help" : _) _ _ = (Help, defaultConfig)
-- termination
-- parseArgs' ("--termination" : fname : []) _ _ = (Termination fname ReductionOrder.GWPO, defaultConfig)
-- parseArgs' ("--termination" : fname : "--lpo" : []) _ _ = (Termination fname ReductionOrder.LPO, defaultConfig)
-- parseArgs' ("--termination" : fname : "--gwpo" : []) _ _ = (Termination fname ReductionOrder.GWPO, defaultConfig)
-- parseArgs' ("--termination" : fname : "--gwpoN" : []) _ _ = (Termination fname ReductionOrder.GWPON, defaultConfig)
-- parseArgs' ("--termination" : fname : "--gwpo01" : []) _ _ = (Termination fname ReductionOrder.GWPO01, defaultConfig)
-- parseArgs' ("--termination" : fname : "--wpo" : []) _ _ = (Termination fname ReductionOrder.WPO, defaultConfig)
-- parseArgs' ("--termination" : fname : "--lpo-via-gwpo" : []) _ _ = (Termination fname ReductionOrder.LPOviaGWPO, defaultConfig)
-- prover
parseArgs' ("--casc" : fname : _) _ _ = (UEQ fname, cascConfig) -- config for CASC UEQ
parseArgs' ("--ueq" : fname : args) _ c = parseArgs' args (UEQ fname) c
-- parseArgs' ("--tptp" : fname : args) _ c = parseArgs' args (TPTP fname) c
parseArgs' ("--inf" : fname : args) _ c = parseArgs' args (INF fname) c
parseArgs' ("--waldmeister" : fname : args) _ c = parseArgs' args (Waldmeister fname) c
-- ordered completion
-- parseArgs' ("--ordered-completion" : fname : args) _ c = parseArgs' args (OrderedCompletion fname) c
-- options
parseArgs' ("--skip-order-search" : n : args) m c = 
  parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = order_class c, verbose = verbose c, termPrinter = termPrinter c, groundCompletion = groundCompletion c, n_skip_order_search = Just (read n) })
parseArgs' ("--no-inter-reduction" : args) m c =
  parseArgs' args m (Config { inter_reduction = False, order_class = order_class c, verbose = verbose c, termPrinter = termPrinter c, groundCompletion = groundCompletion c, n_skip_order_search = n_skip_order_search c })
parseArgs' ("--no-simplification" : args) m c =
  parseArgs' args m (Config { inter_reduction = False, order_class = order_class c, verbose = verbose c, termPrinter = termPrinter c, groundCompletion = groundCompletion c, n_skip_order_search = n_skip_order_search c })
parseArgs' ("--lpo" : args) m c =
  parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = LPO, verbose = verbose c, termPrinter = termPrinter c, groundCompletion = groundCompletion c, n_skip_order_search = n_skip_order_search c })
-- parseArgs' ("--gwpo" : args) m c = parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = GWPO, verbose = verbose c})
-- parseArgs' ("--gwpo01" : args) m c = parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = GWPO01, verbose = verbose c})
-- parseArgs' ("--gwpoN" : args) m c = parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = GWPON, verbose = verbose c})
-- parseArgs' ("--wpo" : args) m c = parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = WPO, verbose = verbose c})
-- parseArgs' ("--lpo-via-gwpo" : args) m c = parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = LPOviaGWPO, verbose = verbose c})
parseArgs' ("--completion-with-parsable-output" : fname : args) _ c = parseArgs' args (CompletionWithParsableOutput fname) c
parseArgs' ("--verbose" : args) m c =
  parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = order_class c, verbose = True, termPrinter = termPrinter c, groundCompletion = groundCompletion c, n_skip_order_search = n_skip_order_search c })
parseArgs' ("--v" : args) m c =
  parseArgs' args m (Config { inter_reduction = inter_reduction c, order_class = order_class c, verbose = True, termPrinter = termPrinter c, groundCompletion = groundCompletion c, n_skip_order_search = n_skip_order_search c })
parseArgs' (fname : args) _ c = parseArgs' args (UEQ fname) c

parseArgs :: [String] -> (Mode, Prover.Config)
parseArgs args = parseArgs' args Help defaultConfig

version :: String
version = "0.6"

notSupported :: IO ()
notSupported = printf "not supported in the version %s.\n" version

handleHelp :: IO ()
handleHelp = do
  putStrLn ("toma version " ++ version)
  putStrLn "Usage: toma <options> <file>"
  putStrLn "<file> is in TPTP (UEQ) format or CoCo INF format."
  -- putStrLn "<file> is in TPTP (UEQ) format, CoCo INF format or TRS format."
  putStrLn "equational theorem proving: "
  putStrLn "$ toma inverse_unit.p        # TPTP UEQ format (simple skolemization)"
  putStrLn "$ toma --ueq  inverse_unit.p # TPTP UEQ format (simple skolemization)"
  putStrLn "$ toma --waldmeister inverse_unit.p # TPTP UEQ format (Waldmeister transformation)"
  putStrLn "$ toma --casc inverse_unit.p        # CASC UEQ mode" 
  -- putStrLn "$ toma --tptp inverse_unit.p # TPTP format (split-if transformation)"
  putStrLn "$ toma --inf  936.trs        # CoCo INF format (split-if transformation)"
  putStrLn "debugging prover:"
  putStrLn "$ toma inverse_unit.p --verbose  # verbose output for equational reasoning"
  putStrLn "$ toma inverse_unit.p --v        # the same as --verbose" 
  putStrLn "no simplification (or no inter-reduction):"
  putStrLn "$ toma inverse_unit.p --no-simplification"
  putStrLn "$ toma inverse_unit.p --no-inter-reduction"
  -- putStrLn "default ordering is --lpo"
  putStrLn "equational theorem proving with various reduciton orders"
  putStrLn "$ toma inverse_unit.p                # the same as --lpo"
  putStrLn "$ toma --lpo inverse_unit.p          # LPO"
  -- putStrLn "$ toma --gwpo inverse_unit.p         # the same as --gwpo01"
  -- putStrLn "$ toma --gwpoN inverse_unit.p        # GWPO with linear polynomial interpretation over N"
  -- putStrLn "$ toma --gwpo01 inverse_unit.p       # GWPO with linear polynomial interpretation over {0, 1} for coefficients and N for constants"
  -- putStrLn "$ toma --wpo inverse_unit.p          # WPO with linear polynomial interpretation over N"
  -- putStrLn "$ toma --lpo-via-gwpo inverse_unit.p # LPO simulation via GWPO"
  -- putStrLn "ordered maximal completion with various reduction orders: "
  -- putStrLn "$ toma --ordered-completion group.trs          # LPO "
  -- putStrLn "$ toma --ordered-completion group.trs --lpo    # LPO"
  -- putStrLn "$ toma --ordered-completion group.trs --gwpo01 # GWPO with linear polynomial interpretation over {0, 1} for coefficients and N for constants"
  -- putStrLn "termination checking with reduction orders: "
  -- putStrLn "$ toma --termination ack.trs                 # the same as --gwpo"
  -- putStrLn "$ toma --termination ack.trs --lpo           # LPO"
  -- putStrLn "$ toma --termination ack.trs --gwpoN         # GWPO with linear polynomial interpretation over N"
  -- putStrLn "$ toma --termination ack.trs --gwpo01        # GWPO with linear polynomial interpretation over {0, 1} for coefficients and N for constants"
  -- putStrLn "$ toma --termination ack.trs --gwpo          # first tries --gwpo01, and then --gwpoN"
  -- putStrLn "$ toma --termination ack.trs --wpo           # WPO with linear polynomial interpretation over N"
  -- putStrLn "$ toma --termination ack.trs --lpo-via-gwpo  # LPO simulation via GWPO"
  putStrLn "$ toma inverse_unit.p --skip-order-search 10 # order search is skipped after 10 iteration.  by default order search is never skippped."
  putStrLn "ordered completion with various reduction orders, with easy-to-parse output of completion."
  putStrLn "$ toma --completion-with-parsable-output group.trs"

-- printTPTPProof :: PES -> Proof -> IO ()
-- printTPTPProof pes (Prover.Join { proof_goal, proof_es, proof_deleted_es }) = do
--   putStrLn "% SZS status Unsatisfiable"
--   putStrLn "% SZS output start Proof"
--   putStrLn "The given problem is transformed into a word problem"
--   putStrLn "by the split-if encoding (Claessen and Smallbone, 2018)."
--   putStrLn "To show the unsatisfiability,"
--   putStrLn "it suffices to show that true__ = false__ is valid in the following ES:"
--   putStrLn (showPES pes)
--   putStrLn "This is an equational proof of true__ = false__:"
--   putStrLn (showES (sortById (relevant (eqn_id proof_goal) (proof_goal : proof_es ++ proof_deleted_es))))
--   putStrLn "% SZS output end Proof"
-- printTPTPProof pes (Prover.Complete { proof_es, proof_reduction_order_param }) = do
--   putStrLn "% SZS status Satisfiable"
--   putStrLn "% SZS output start Proof"
--   putStrLn "The given problem is transformed into a word problem"
--   putStrLn "by the split-if encoding (Claessen and Smallbone, 2018)."
--   putStrLn "To show the satisfiability,"
--   putStrLn "it suffices to show that true__ = false__ is not valid in the following ES:"
--   putStrLn (showPES pes)
--   putStrLn "The ES admits the following ground-complete ordered rewrite system:"
--   putStrLn (show proof_reduction_order_param)
--   putStrLn "ES:"
--   putStrLn (showOTRS (param2ord proof_reduction_order_param) proof_es)
--   putStrLn "Since true__ and false__ are not joinable in the ordered rewrite system,"
--   putStrLn " true__ = false__ is not valid."
--   putStrLn "% SZS output end Proof"
-- printTPTPProof _ Failure = putStrLn "% SZS status GaveUp : proof failed"

-- printTPTPTrivial :: IO ()
-- printTPTPTrivial = do
--   putStrLn "% SZS status Satisfiable"
--   putStrLn "% SZS output Proof"
--   putStrLn "% The given problem admits an trivial model."
--   putStrLn "% SZS output end Proof"

-- TODO: suppert 'include' from $TPTP 
handleTPTP :: String -> Prover.Config -> IO ()
handleTPTP _fname _conf = notSupported
-- handleTPTP fname conf = do
--   result <- readTPTP fname
--   case result of
--     Left e -> do
--       putStdErr "% SZS status SyntaxError"
--       putStdErr (show e)
--     Right tptp -> do
--       case splitIfTPTP tptp of
--         Nothing      -> do
--           putStdErr "% SZS status UsageError : The input contains a non-Horn-clause."
--         Just (es, goal@(s, t)) -> do
--           putStr (showTPTP (convertES es ++ [convertGoal goal]))
--           if s == t -- trivial model found
--             then printTPTPTrivial
--             else do p <- prove conf goal (axioms es)
--                     printTPTPProof es p

separate :: [AnnotatedFormula] -> ([PT.TermPair], [PT.TermPair])
separate [] = ([], [])
separate ((_, _, (Eq, pe)) : afs) =
  (pe : pes, gs)
  where (pes, gs) = separate afs
separate ((_, _, (Neq, g)) : afs) = 
  (pes, g : gs)
  where (pes, gs) = separate afs

skolemize :: [PT.Function] -> (PT.Term, PT.Term) -> (PT.Term, PT.Term)
skolemize fs e@(s, t) = (PT.substitute s sigma, PT.substitute t sigma)
  where
    xs = PT.varsInPair e
    cs = [ c | i <- [1 :: Int ..], let c = "c" ++ show i, notElem c fs ]
    sigma = [ (x, PT.F c []) | (x, c) <- zip xs cs ]

syms :: String -> [String]
syms s = s : [s ++ show i | i <- [0 :: Int .. ] ]

-- transforms a (possiblely non-ground) goal s = t
-- into __true = __false
-- by adding __eq(x, x) = __true
--           __eq(s, t) = __false
waldmeister :: [PT.TermPair] -> PT.TermPair -> ([PT.TermPair], PT.TermPair)
waldmeister pes goal@(s, t) = (eqAxiom : gAxiom : pes, (tTerm, fTerm))
  where
    fs = [ f  | e <- goal : pes, f <- PT.functionsInPair e]
    eqSymbol = head [ c | c <- syms "__eq" , notElem c fs]
    tSymbol = head [ c | c <- syms "__true" , notElem c fs]
    fSymbol = head [ c | c <- syms "__false" , notElem c fs]
    tTerm = PT.F tSymbol []
    fTerm = PT.F fSymbol []
    eqAxiom = (PT.F eqSymbol [PT.V "X", PT.V "X"], tTerm)
    gAxiom = (PT.F eqSymbol [s, t], fTerm)

-- add fake goal for ordered completion
-- TODO: If there is variable with the same name as __true or __false, 
--       we need to rename the variable.
fakeGoal :: PT.TermPair
fakeGoal =
  ( PT.F "___true"  []
  , PT.F "___false" [])

includeDirectories :: IO [String]
includeDirectories = do
  env <- getEnvironment
  case List.lookup "TPTP" env of
    Nothing  -> return ["."]
    Just dir -> return [".", dir]

printUEQProof :: TermPrinter -> ([PT.TermPair], PT.TermPair) -> PE -> Proof -> IO ()
printUEQProof tp (es, goal) new_goal (Prover.Join { proof_goal, proof_es, proof_deleted_es}) = do
  putStrLn "% SZS status Unsatisfiable"
  putStrLn "% SZS output start Proof"
  putStrLn "original problem:"
  putStrLn "axioms:"
  BSB.hPutBuilder stdout (showAxioms es <> BSB.string7 "\n")
  putStrLn "goal:"
  BSB.hPutBuilder stdout (showNegatedLiteral goal <> BSB.string7 "\n")
  putStrLn "To show the unsatisfiability of the original goal,"
  BSB.hPutBuilder stdout (BSB.string7 "it suffices to show that " <> showPE tp new_goal <> BSB.string7 " (skolemized goal) is valid under the axioms.\n")
  putStrLn "Here is an equational proof:"
  BSB.hPutBuilder stdout (showES tp (sortById (relevant (eqn_id proof_goal) (proof_goal : proof_es ++ proof_deleted_es))) <> BSB.string7 "\n")
  putStrLn "% SZS output end Proof"
printUEQProof tp  (es, goal) _new_goal (Prover.Complete { proof_es, proof_reduction_order_param })
  | PT.isGroundTermPair goal = do -- So goal = _new_goal holds.
      putStrLn "% SZS status Satisfiable"
      putStrLn "% SZS output start Proof"
      putStrLn "original problem:"
      putStrLn "axioms:"
      BSB.hPutBuilder stdout (showAxioms es <> BSB.string7 "\n")
      putStrLn "goal:"
      BSB.hPutBuilder stdout (showNegatedLiteral goal <> BSB.string7 "\n")
      putStrLn "To show the satisfiability of the original goal,"
      BSB.hPutBuilder stdout (BSB.string7 "it suffices to show that " <> showPositiveLiteral goal <> BSB.string7 " is not valid under the following axioms:\n")
      putStrLn "The new axioms admits the following ground-complete ordered rewrite system:"
      BSB.hPutBuilder stdout (showReductionOrderParam (functionPrinter tp) proof_reduction_order_param <> BSB.string7 "\n")
      putStrLn "ES:"
      BSB.hPutBuilder stdout (showOTRS tp (param2ord proof_reduction_order_param) proof_es <> BSB.string7 "\n")
      BSB.hPutBuilder stdout (showPositiveLiteral goal  <> BSB.string7 " is not joinable.\n")
      BSB.hPutBuilder stdout (BSB.string7 "So " <> showPositiveLiteral goal <> BSB.string7 " is not valid.\n")
      putStrLn "% SZS output end Proof"
  | otherwise = do
      putStrLn "% SZS status GaveUp : the original goal is not ground"
      putStrLn "However the axioms admit the following ground-complete ordered rewrite system:"
      BSB.hPutBuilder stdout (showReductionOrderParam (functionPrinter tp) proof_reduction_order_param <> BSB.string7 "\n")
      putStrLn "ES:"
      BSB.hPutBuilder stdout (showOTRS tp (param2ord proof_reduction_order_param) proof_es <> BSB.string7 "\n")
printUEQProof _ _ _ Failure = putStrLn "% SZS status GaveUp : proof failed"

-- pure equational theory is always consistent in the sense of first-order logic
-- since it has trivial model (model with one element).
-- Namely it is satisfiable in the sense of first-order logic.
handleNoGoal :: IO ()
handleNoGoal = do
  putStrLn "% SZS status Satisfiable"
  putStrLn "% SZS output start Proof"
  putStrLn "The theory has the trivial model."
  putStrLn "% SZS output end Proof"

handleTooManyGoals :: IO ()
handleTooManyGoals = putStdErr "% SZS status UsageError : number of goals must be exactly one"

handleUEQ :: String -> Prover.Config -> IO ()
handleUEQ filename conf = do
  dirs <- includeDirectories
  result <- readUEQFile dirs filename
  case result of
    Left e -> do
      putStdErr "% SZS status SyntaxError"
      putStdErr (show e)
    Right afs -> do
      let (pes, goals) = separate afs
      case goals of
        [goal] -> do
          let fs = PT.functionsInES (goal : pes)
          let skolemized_goal = skolemize fs goal
          let fs' = PT.functionsInES (skolemized_goal : pes)
          let vs = PT.varsInES pes
          let fd = zip fs' [0..]
          let vd = zip vs [0..]
          let fd' = [ (f', BSB.string7 f) | (f, f') <- fd ]
          let tp = (Map.fromList fd', BSB.string7 "X")
          let es = axioms [ toTermPair fd vd e | e <- pes ]
          let goal' = toTermPair fd vd skolemized_goal
          p <- prove (setTermPrinter tp conf) goal' es
          printUEQProof tp (pes, goal) goal'  p
        [] -> handleNoGoal
        _ -> handleTooManyGoals

showAxioms :: [PT.TermPair] -> BSB.Builder
showAxioms axs =
  mconcat (List.intersperse (BSB.string7 "\n") [ PT.showTerm s <> BSB.string7 " = " <> PT.showTerm t | (s, t) <- axs ])

-- show axioms parse-easy format
-- print index of the axiom
showAxiomsParsable :: TermPrinter -> ES -> BSB.Builder
showAxiomsParsable tp axs =
  mconcat (List.intersperse (BSB.string7 "\n") [ show tp e | e <- axs ])
  where show :: TermPrinter -> E -> BSB.Builder
        show p (E { eqn = (l, r), eqn_id = i, eqn_derivation = d  })
          = (BSB.intDec i) <> (BSB.string7 ": ") <> Term.showTerm p l <> (BSB.string7 " = ") <> Term.showTerm p r <> (BSB.string7 ".")

showNegatedLiteral :: PT.TermPair -> BSB.Builder
showNegatedLiteral (s, t) = PT.showTerm s <> BSB.string7 " != " <> PT.showTerm t

showPositiveLiteral :: PT.TermPair -> BSB.Builder
showPositiveLiteral (s, t) = PT.showTerm s <> BSB.string7 " = " <> PT.showTerm t

printWaldmeisterProof :: TermPrinter -> ([PT.TermPair], PT.TermPair) -> (PES, PE) -> Proof -> IO ()
printWaldmeisterProof tp (es, goal) (new_es, new_goal) (Prover.Join { proof_goal, proof_es, proof_deleted_es}) = do
  putStrLn "% SZS status Unsatisfiable"
  putStrLn "% SZS output start Proof"
  putStrLn "original problem:"
  putStrLn "axioms:"
  BSB.hPutBuilder stdout ((showAxioms es) <> BSB.string7 "\n")
  putStrLn "goal:"
  BSB.hPutBuilder stdout (showNegatedLiteral goal <> BSB.string7 "\n")
  putStrLn "To show the unsatisfiability of the original goal,"
  BSB.hPutBuilder stdout (BSB.string7 "it suffices to show that " <> showPE tp new_goal <> BSB.string7 " is valid under the axioms.\n")
  putStrLn "new axioms:"
  BSB.hPutBuilder stdout (showPES tp new_es <> BSB.string7 "\n")
  putStrLn "Here is an equational proof:"
  BSB.hPutBuilder stdout (showES tp (sortById (relevant (eqn_id proof_goal) (proof_goal : proof_es ++ proof_deleted_es))) <> BSB.string7 "\n")
  putStrLn "% SZS output end Proof"
printWaldmeisterProof tp  (es, goal) (new_es, new_goal) (Prover.Complete { proof_es, proof_reduction_order_param }) = do
  putStrLn "% SZS status Satisfiable"
  putStrLn "% SZS output start Proof"
  putStrLn "original problem:"
  putStrLn "axioms:"
  BSB.hPutBuilder stdout (showAxioms es <> BSB.string7 "\n")
  putStrLn "goal:"
  BSB.hPutBuilder stdout (showNegatedLiteral goal <> BSB.string7 "\n")
  putStrLn "To show the satisfiability of the original goal,"
  BSB.hPutBuilder stdout (BSB.string7 "it suffices to show that the new formula " <> showPE tp new_goal <> BSB.string7 " is not valid under the following axioms:\n")
  putStrLn "new axioms:"
  BSB.hPutBuilder stdout (showPES tp new_es <> BSB.string7 "\n")
  putStrLn "The new axioms admits the following ground-complete ordered rewrite system:"
  BSB.hPutBuilder stdout (showReductionOrderParam (functionPrinter tp) proof_reduction_order_param <> BSB.string7 "\n")
  putStrLn "ES:"
  BSB.hPutBuilder stdout (showOTRS tp (param2ord proof_reduction_order_param) proof_es <> BSB.string7 "\n")
  BSB.hPutBuilder stdout (showPE tp new_goal <> BSB.string7 " is not joinable.\n")
  BSB.hPutBuilder stdout (BSB.string7 "So " <> showPE tp new_goal <> BSB.string7 " is not valid.\n")
  putStrLn "% SZS output end Proof"
printWaldmeisterProof _ _ _ Failure = putStrLn "% SZS status GaveUp : proof failed"

toTerm :: [(PT.Function, Term.Function)] -> [(PT.Var, Term.Var)] -> PT.Term -> Term.Term
toTerm _fd vd (PT.V x) = Term.var (Util.unsafeLookup x vd)
toTerm fd vd (PT.F f ts) =
  Term.app (Util.unsafeLookup f fd) [ toTerm fd vd t | t <- ts]

toTermPair :: [(PT.Function, Term.Function)] -> [(PT.Var, Term.Var)] -> PT.TermPair -> Term.TermPair
toTermPair fd vd (s, t) = (toTerm fd vd s, toTerm fd vd t)

setTermPrinter :: Term.TermPrinter -> Prover.Config -> Prover.Config
setTermPrinter p conf = 
  Prover.Config { inter_reduction = inter_reduction conf,
                  order_class = order_class conf,
                  verbose = verbose conf,
                  termPrinter = p,
                  groundCompletion = groundCompletion conf ,
                  n_skip_order_search = n_skip_order_search conf }

-- same as handleUEQ, exept for transformation waldmeister
handleWaldmeister :: String -> Prover.Config -> IO ()
handleWaldmeister filename conf = do
  dirs <- includeDirectories
  result <- readUEQFile dirs filename
  case result of
    Left e -> do
      putStdErr "% SZS status SyntaxError"
      putStdErr (show e)
    Right afs -> do
      let (pes, goals) = separate afs
      case goals of
        [goal] -> do
          let (pes', goal') = waldmeister pes goal
          let fs = PT.functionsInES (goal' : pes')
          let vs = PT.varsInES (goal' : pes')
          let fd = zip fs [0..]
          let vd = zip vs [0..]
          let fd' = [ (f', BSB.string7 f) | (f, f') <- fd ]
          let tp = (Map.fromList fd', BSB.string7 "X")
          let axs = axioms [ toTermPair fd vd e | e <- pes' ]
          let goal'' = toTermPair fd vd goal'
          p <- prove (setTermPrinter tp conf) goal'' axs
          printWaldmeisterProof tp (pes, goal) (toPES axs, goal'') p
        [] -> handleNoGoal
        _ -> handleTooManyGoals

printINFProof :: TermPrinter -> ([PT.TermPair], PT.TermPair) -> PE -> Proof -> IO ()
printINFProof tp (es, _goal) _new_goal (Complete {proof_es, proof_reduction_order_param} ) = do
  -- new_goal must be __true = __false ?
  putStrLn "YES"
  putStrLn "Proof: "
  putStrLn "To show the infeasibility of the given CoCo INF problem, "
  putStrLn "we transform the problem into a word problem by the split-if encoding (Claessen and Smallbone, 2018)."
  putStrLn "It suffices to show that the follwing ES does not entail true__ = false__ ."
  putStrLn "ES:"
  BSB.hPutBuilder stdout (showAxioms es <> BSB.string7 "\n")
  putStrLn "The ES admits the following equivalent ground-complete ordered rewrite system:"
  BSB.hPutBuilder stdout (showReductionOrderParam (functionPrinter tp) proof_reduction_order_param <> BSB.string7 "\n")
  putStrLn "ES:"
  BSB.hPutBuilder stdout (showOTRS tp (param2ord proof_reduction_order_param) proof_es <> BSB.string7 "\n")
  putStrLn "Since true__ and false__ are not joinable,"
  putStrLn "the original problem is infeasible."
printINFProof _ _ _ (Prover.Join {}) = putStrLn "MAYBE"
printINFProof _ _ _ Failure = putStrLn "MAYBE"

handleINF :: String -> Prover.Config -> IO ()
handleINF fname conf = do
  result <- readINFProblem fname
  case result of
    Left e -> do
      putStdErr "ERROR"
      putStdErr (show e)
    Right inf -> do
      case splitIfINF inf of
        Nothing      -> putStdErr "ERROR"
        Just (pes, goal@(s, t)) -> do
          if s == t -- trivial model found
            then do putStrLn "MAYBE" -- TODO: print YES with an horn clauses that admits a trivial model.
                    -- putStrLn "Proof:"
                    -- putStrLn "The given CoCo INF problem is transformed into a satisfiability problem of horn clauses."
                    -- -- TODO: we need to print the horn clauses here...
                    -- putStrLn "Since the horn clauses has a trivial model,"
                    -- putStrLn "the original problem is infeasible."
            else do
              let fs = PT.functionsInES (goal : pes)
              let skolemized_goal = skolemize fs goal -- is this neccesary?
              let fs' = PT.functionsInES (skolemized_goal : pes)
              let vs = PT.varsInES pes
              let fd = zip fs' [0..]
              let vd = zip vs [0..]
              let fd' = [ (f', BSB.string7 f) | (f, f') <- fd ]
              let tp = (Map.fromList fd', BSB.string7 "X")
              let es = axioms [ toTermPair fd vd e | e <- pes ]
              let goal' = toTermPair fd vd skolemized_goal
              p <- prove (setTermPrinter tp conf) goal' es
              printINFProof tp (pes, goal) goal'  p

-- printOrderedCompletionProof :: PE -> Proof -> IO ()
-- printOrderedCompletionProof fake_goal (Prover.Join {}) = do
--   putStrLn "ERROR"
--   putStrLn ("The joinability of the fake goal " ++ (showPE fake_goal) ++ " implies the existence of a bug.")
-- printOrderedCompletionProof fake_goal (Prover.Complete { proof_es, proof_reduction_order_param }) = do
--   putStrLn "YES"
--   putStrLn "The ES admits the following ground-complete ordered rewrite system:"
--   putStrLn (show proof_reduction_order_param)
--   putStrLn "ES:"
--   putStrLn (showOTRS (param2ord proof_reduction_order_param) proof_es)
--   putStrLn ""
--   putStrLn ("Note that new symbols are introduced by the fake goal " ++ (showPE fake_goal) ++ ".")
-- printOrderedCompletionProof _ Failure = do
--   putStrLn "ERROR"
--   putStrLn "completion failed"

handleOrderedCompletion :: String -> Prover.Config -> IO ()
handleOrderedCompletion _ _ = notSupported
-- handleOrderedCompletion fname conf = do
--   result <- readTRSFile fname
--   case result of
--     Left e -> do
--       putStdErr "ERROR"
--       putStdErr (show e)
--     Right (pes, _mu) -> do
--       let axs = axioms pes
--       let fake_goal = fakeGoal pes
--       p <- prove conf fake_goal axs
--       printOrderedCompletionProof fake_goal p

handleTermination :: String -> ReductionOrder.Class -> IO ()
handleTermination _fname _conf = notSupported
-- handleTermination fname conf = do
--   parseres <- readTRSFile fname
--   case parseres of
--     Left e -> do
--       putStdErr "ERROR"
--       putStdErr (show e)
--     Right (trs, _mu) ->
--       terminationChecker conf trs

printCompletionProof :: TermPrinter -> ES -> Proof -> IO ()
printCompletionProof tp _ (Prover.Join { proof_goal, proof_es, proof_deleted_es}) = do
  putStrLn "FAIL"
  putStrLn "fake goal joined. someting is wrong."
printCompletionProof tp es (Prover.Complete { proof_es, proof_reduction_order_param, proof_deleted_es }) = do
  putStrLn "Completed"
  putStrLn "axioms:"
  BSB.hPutBuilder stdout (showAxiomsParsable tp es <> BSB.string7 "\n")
  putStrLn "Here is an equational proof:"
  let sorted_es = (sortById (proof_es ++ proof_deleted_es))
  -- BSB.hPutBuilder stdout (showES tp (sortById (relevant (eqn_id proof_goal) (proof_goal : proof_es ++ proof_deleted_es))) <> BSB.string7 "\n")
  BSB.hPutBuilder stdout (showES tp sorted_es <> BSB.string7 "\n")
  putStrLn "ES:"
  BSB.hPutBuilder stdout (showOTRS tp (param2ord proof_reduction_order_param) proof_es <> BSB.string7 "\n")
printCompletionProof _ _ Failure = putStrLn "% SZS status GaveUp : proof failed"

handleCompletionWithParsableOutput :: String -> Prover.Config -> IO ()
handleCompletionWithParsableOutput _fname _conf = do
  result <- readTRSFile _fname
  case result of
    Left e -> do
      putStdErr "ERROR"
      putStdErr (show e)
    Right (pes, _) -> do
      let fs = PT.functionsInES pes
      let skolemized_goal = skolemize fs fakeGoal
      let fs' = PT.functionsInES (fakeGoal : pes)
      let vs = PT.varsInES pes
      let fd = zip fs' [0..]
      let vd = zip vs [0..]
      let fd' = [ (f', BSB.string7 f) | (f, f') <- fd ]
      let tp = (Map.fromList fd', BSB.string7 "X")
      let es = axioms [ toTermPair fd vd e | e <- pes ]
      let goal' = toTermPair fd vd skolemized_goal
      p <- prove (setTermPrinter tp _conf) goal' es
      printCompletionProof tp es p

dispatch :: (Mode, Prover.Config) -> IO ()
dispatch (Help, _conf) = handleHelp
dispatch (UEQ fname, conf) = handleUEQ fname conf
dispatch (Waldmeister fname, conf) = handleWaldmeister fname conf
dispatch (TPTP fname, conf) = handleTPTP fname conf
dispatch (INF fname, conf) = handleINF fname conf
dispatch (OrderedCompletion fname, conf) = handleOrderedCompletion fname conf
dispatch (Termination fname tconf, _conf) = handleTermination fname tconf
dispatch (CompletionWithParsableOutput fname, conf) = handleCompletionWithParsableOutput fname conf

main :: IO ()
main = do
  args <- getArgs
  dispatch (parseArgs args)
