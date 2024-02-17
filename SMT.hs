module SMT where
-- TODO: remove SMT module, and move everything to Z3.

import Data.List
import Util
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration

data Exp = Var String
         | Val Int
         | Plus [Exp]
         | Times [Exp]
         | AndE [Exp] -- boolean experssion
         | Ite Exp Exp Exp -- (ite e1 e2 e3) : if-then-else
         deriving Eq

data Formula = And [Formula]
             | Or [Formula]
             | Not Formula
             | Distinct [Exp]
             | Gt Exp Exp
             | Geq Exp Exp
             | Gtn [Exp] -- (> t1 t2 t3 ... tn)
             | Eq Exp Exp
             | BoolVar String
             deriving Eq

instance Show Exp where
    show (Var s)     = s 
    show (Val n)       = show n
    show (Plus []) = "0"
    show (Plus [e]) = show e
    show (Plus es)  = "(+ " ++ intercalate " " [show e | e <- es] ++ ")"
    show (Times []) = "1"
    show (Times [e]) = show e
    show (Times es)  = "(* " ++ intercalate " " [show e | e <- es] ++ ")"
    show (AndE []) = "true"
    show (AndE [e]) = show e
    show (AndE es)  = "(and " ++ (intercalate " " [ show e | e <- es ]) ++ ")"
    show (Ite (AndE []) e2 _e3) = show e2
    show (Ite e1 e2 e3) = "(ite " ++  (intercalate " " [ show e | e <- [e1, e2, e3] ]) ++ ")"

showExp :: Exp -> BSB.Builder
showExp (Var s) = BSB.string7 s
showExp (Val n) = BSB.intDec n
showExp (Plus []) = BSB.string7 "0"
showExp (Plus [e]) = showExp e
showExp (Plus es)  = BSB.string7 "(+ " <> mconcat (intersperse (BSB.string7 " ") [showExp e | e <- es]) <> BSB.string7 ")"
showExp (Times []) = BSB.string7 "1"
showExp (Times [e]) = showExp e
showExp (Times es)  = BSB.string7 "(* " <> mconcat (intersperse (BSB.string7 " ") [showExp e | e <- es]) <> BSB.string7 ")"
showExp (AndE []) = BSB.string7 "true"
showExp (AndE [e]) = showExp e
showExp (AndE es)  = BSB.string7 "(and " <> mconcat (intersperse (BSB.string7 " ") [ showExp e | e <- es ]) <> BSB.string7 ")"
showExp (Ite (AndE []) e2 _e3) = showExp e2
showExp (Ite e1 e2 e3) = BSB.string7 "(ite " <>  mconcat (intersperse (BSB.string7 " ") [ showExp e | e <- [e1, e2, e3] ]) <> BSB.string7 ")"

instance Show Formula where
    show (BoolVar s) = s
    show (And []) = "true"
    show (And [f]) = show f
    show (And fs) = "(and " ++ intercalate " " [show f | f <- fs] ++ ")"
    show (Or []) = "false"
    show (Or [f]) = show f
    show (Or fs) = "(or " ++ intercalate " " [show f | f <- fs] ++ ")"
    show (Not f) = "(not " ++ show f ++ ")"
    show (Distinct es) =  "(distinct " ++ intercalate " " [show e | e <- es] ++ ")"
    show (Gt e1 e2) = "(> " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Geq e1 e2) = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Gtn es) =  "(> " ++ intercalate " " [show e | e <- es] ++ ")"
    show (Eq e1 e2) = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"

showFormula :: Formula -> BSB.Builder
showFormula (BoolVar s) = BSB.string7 s
showFormula (And []) = BSB.string7 "true"
showFormula (And [f]) = showFormula f
showFormula (And fs) = BSB.string7 "(and " <> mconcat (intersperse (BSB.string7 " ") [showFormula f | f <- fs]) <> BSB.string7 ")"
showFormula (Or []) = BSB.string7 "false"
showFormula (Or [f]) = showFormula f
showFormula (Or fs) = BSB.string7 "(or " <> mconcat (intersperse (BSB.string7 " ") [showFormula f | f <- fs]) <> BSB.string7 ")"
showFormula (Not f) = BSB.string7 "(not " <> showFormula f <> BSB.string7 ")"
showFormula (Distinct es) = BSB.string7 "(distinct " <> mconcat (intersperse (BSB.string7 " ") [showExp e | e <- es]) <> BSB.string7 ")"
showFormula (Gt e1 e2) = BSB.string7 "(> " <> showExp e1 <> BSB.string7 " " <> showExp e2 <> BSB.string7 ")"
showFormula (Geq e1 e2) = BSB.string7 "(>= " <> showExp e1 <> BSB.string7 " " <> showExp e2 <> BSB.string7 ")"
showFormula (Gtn es) = BSB.string7 "(> " <> mconcat (intersperse (BSB.string7 " ") [showExp e | e <- es]) <> BSB.string7 ")"
showFormula (Eq e1 e2) = BSB.string7 "(= " <> showExp e1 <> BSB.string7 " " <> showExp e2 <> BSB.string7 ")"

data Command =
     DeclareInt String
   | DeclareBool String
   | Assert Formula
   | AssertSoft Formula
   | CheckSat
   | GetValue [String]

type SMTInput = [Command]

data SMTValue = SMTBoolValue Bool | SMTIntValue Int
                deriving Show

type Model = [(String, SMTValue)]

-- TODO: distinguish error and unsat...
type SMTOutput = Maybe Model

instance Show Command where
    show (DeclareInt varname) = "(declare-const "  ++ varname ++ " Int)"
    show (DeclareBool varname) = "(declare-const "  ++ varname ++ " Bool)"
    show (Assert f) = "(assert " ++ show f ++ ")"
    show (AssertSoft f) = "(assert-soft " ++ show f ++ ")"
    show CheckSat = "(check-sat)"
    show (GetValue vars) = "(get-value (" ++ intercalate " " [ v | v <- vars ] ++ "))"

showCommand :: Command -> BSB.Builder
showCommand (DeclareInt varname) = BSB.string7 "(declare-const "  <> BSB.string7 varname <> BSB.string7 " Int)"
showCommand (DeclareBool varname) = BSB.string7 "(declare-const "  <> BSB.string7 varname <> BSB.string7 " Bool)"
showCommand (Assert f) = BSB.string7 "(assert " <> showFormula f <> BSB.string7 ")"
showCommand (AssertSoft f) = BSB.string7 "(assert-soft " <> showFormula f <> BSB.string7 ")"
showCommand CheckSat = BSB.string7 "(check-sat)"
showCommand (GetValue vars) = BSB.string7 "(get-value (" <> mconcat (intersperse (BSB.string7 " ") [ BSB.string7 v | v <- vars ]) <> BSB.string7 "))"

-- smart constructors

andFormula :: [Formula] -> Formula
andFormula [f] = f -- propagete partial evaluation
andFormula fs
  | falseFormula `elem` fs' = falseFormula
  | otherwise = case fs' of
                  [] -> trueFormula
                  _  -> And fs'
  where fs' = [f | f <- fs, f /= trueFormula]

orFormula :: [Formula] -> Formula
orFormula [f] = f -- propagate partial evaluation
orFormula fs
  | trueFormula `elem` fs' = trueFormula
  | otherwise = case fs' of
                  [] -> falseFormula
                  _  -> Or fs'
  where fs' = [f | f <- fs, f /= falseFormula]

notFormula :: Formula -> Formula
notFormula f
  | f == trueFormula = falseFormula
  | f == falseFormula = trueFormula
  | otherwise = Not f

impliesFormula :: Formula -> Formula -> Formula
impliesFormula p q = orFormula [notFormula p, q]

trueFormula :: Formula
trueFormula = And []

falseFormula :: Formula
falseFormula = Or []

isSMTBoolTrue :: SMTValue -> Bool
isSMTBoolTrue (SMTBoolValue True) = True
isSMTBoolTrue _ = False

-- caution: this function ignores bool value.
fromSMTValue2Int :: SMTValue -> [Int]
fromSMTValue2Int (SMTIntValue n) = [n]
fromSMTValue2Int _ = []

smtValueUnsafeCastInt :: SMTValue -> Int
smtValueUnsafeCastInt (SMTIntValue i) = i
-- NOTE: this behaviour seems confusing...
smtValueUnsafeCastInt (SMTBoolValue True) = 1
smtValueUnsafeCastInt (SMTBoolValue False) = 0

showSMTInput :: SMTInput -> BSB.Builder
showSMTInput commands = mconcat (intersperse (BSB.string7 "\n") [showCommand c | c <- commands])

generateSMTInput :: [String] -> [String] -> Formula -> SMTInput
generateSMTInput ivs bvs formula = commands
  where commands =  [DeclareInt v | v <- ivs]
                 ++ [DeclareBool v | v <- bvs]
                 ++ [Assert formula, CheckSat, GetValue ivs ]

maxsat :: [String] -> [String] -> [Formula] -> Formula -> SMTInput
maxsat ivs bvs ss h = commands
  where commands =  [DeclareInt v | v <- ivs]
                 ++ [DeclareBool v | v <- bvs]
                 ++ [AssertSoft s | s <- ss]
                 ++ [Assert h, CheckSat, GetValue (ivs ++ bvs)]

sat :: [String] -> [String] -> Formula -> SMTInput
sat ivs bvs h = maxsat ivs bvs [] h

-- renaming

rename :: String -> [(String, String)] -> String
rename x dic =
  case lookup x dic of
    Just y -> y
    Nothing -> error "rename: not found"

variables :: SMTInput -> [String]
variables [] = []
variables ((DeclareInt v) : cs) = v : variables cs
variables ((DeclareBool v) : cs) = v : variables cs
variables (_ : cs) = variables cs

renameExp :: Exp -> [(String, String)] -> Exp
renameExp (Var x) dic = Var (rename x dic)
renameExp v@(Val _) _dic =  v
renameExp (Plus es) dic = Plus [ renameExp e dic | e <- es]
renameExp (Times es) dic = Times [ renameExp e dic | e <- es]
renameExp (AndE es) dic = AndE [ renameExp e dic | e <- es]
renameExp (Ite e1 e2 e3) dic = Ite (renameExp e1 dic) (renameExp e2 dic) (renameExp e3 dic)

renameFormula :: Formula -> [(String, String)] -> Formula
renameFormula (And fs) dic = And [ renameFormula f dic | f <- fs ]
renameFormula (Or fs) dic = Or [ renameFormula f dic | f <- fs ]
renameFormula (Not f) dic = Not (renameFormula f dic)
renameFormula (Distinct es) dic = Distinct [ renameExp e dic | e <- es]
renameFormula (Gt e1 e2) dic = Gt (renameExp e1 dic) (renameExp e2 dic)
renameFormula (Geq e1 e2) dic = Geq (renameExp e1 dic) (renameExp e2 dic)
renameFormula (Gtn es) dic = Gtn [ renameExp e dic | e <- es ]
renameFormula (Eq e1 e2) dic = Eq (renameExp e1 dic) (renameExp e2 dic)
renameFormula (BoolVar x) dic = BoolVar (rename x dic)

renameSMTInput :: SMTInput -> [(String, String)] -> SMTInput
renameSMTInput [] _ = []
renameSMTInput ((DeclareInt x) : cs) dic
  = (DeclareInt (rename x dic)) : (renameSMTInput cs dic)
renameSMTInput ((DeclareBool x) : cs) dic
  = (DeclareBool (rename x dic)) : (renameSMTInput cs dic)
renameSMTInput ((Assert f) : cs) dic
  = (Assert (renameFormula f dic)) : (renameSMTInput cs dic)
renameSMTInput ((AssertSoft f) : cs) dic
  = (AssertSoft (renameFormula f dic)) : (renameSMTInput cs dic)
renameSMTInput (CheckSat : cs) dic = CheckSat : (renameSMTInput cs dic)
renameSMTInput (GetValue xs : cs) dic
  = (GetValue [ rename x dic | x <- xs ]) : (renameSMTInput cs dic)

evalModel :: String -> Model -> Int
evalModel s m = smtValueUnsafeCastInt (Util.unsafeLookup s m)
