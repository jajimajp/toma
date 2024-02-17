module Term where

import qualified Data.ByteString.Builder as BSB
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import MapUtil

type Var = Int
type Function = Int
type Size = Int

 -- should not be list, but not so bad?
 -- (providing that there would not be function symbols with arity > 20 or so)
data Term = V Var Size
          | F Function [Term] Size

type TermPair = (Term, Term)

-- Let gt :: TermOrder. 
-- gt s t = True means s > t.
type TermOrder = Term -> Term -> Bool

type VariablePrinter = BSB.Builder -- prefix of variables
type FunctionPrinter = Map.Map Function BSB.Builder 
type TermPrinter = (FunctionPrinter, VariablePrinter)

functionPrinter :: TermPrinter -> FunctionPrinter
functionPrinter = fst

variablePrinter :: TermPrinter -> VariablePrinter
variablePrinter = snd

showTerm :: TermPrinter -> Term -> BSB.Builder
showTerm (_mf, prefix) (V x _) = mconcat [prefix, BSB.intDec x]
showTerm p@(mf, _) (F f ts _) =
  (unsafeLookup f mf) <> (BSB.string7 "(") <> mconcat (List.intersperse (BSB.string7 ", ") [ showTerm p t | t <- ts ]) <> (BSB.string7 ")")

-- smart constructors
constant :: Function -> Term
constant f = F f [] 1

var :: Var -> Term
var i = V i 1

app :: Function -> [Term] -> Term
app f ts = F f ts (1 + sum [ size t | t <- ts])

size :: Term -> Size
size (V _ s) = s
size (F _ _ s) = s

instance Eq Term where
  V x _ == V y _ = x == y
  (F _ _ _) == (V _ _) = False
  (V _ _) == (F _ _ _) = False
  (F f1 ts1 s1) == (F f2 ts2 s2) = f1 == f2 && s1 == s2 && all (\(t1, t2) -> t1 == t2) (zip ts1 ts2)

type Position = [Int]

emptyPosition :: Position
emptyPosition = []

-- Var(t)
variables :: Term -> [Var]
variables (V x _)    = [x]
variables (F _f ts _) = List.nub [ x | ti <- ts, x <- variables ti ]

isGround :: Term -> Bool
isGround t = null (variables t)

varsInList :: [Term] -> [Var]
varsInList ts = List.nub [ v | t <- ts, v <- variables t]

varsInPair :: TermPair -> [Var]
varsInPair (t, s) = varsInList [t, s]

-- returns an infinite list of fresh variables
freshVariables :: [Term] -> [Var]
freshVariables ts = [f..]
  where
    f = maximum (varsInList ts) + 1

functions :: Term -> [Function]
functions (V _ _) = []
functions (F f ts _) = List.nub (f : [f' | t <- ts, f' <- functions t])

functionsInPair :: TermPair -> [Function]
functionsInPair (t, s) = List.nub (functions t ++ functions s)

-- NOTE: this function assumes that arity is consistent.
functionsWithArity :: Term -> [(Function, Int)]
functionsWithArity (V _ _) = []
functionsWithArity (F f ts _) =
  List.nub ((f, length ts) : [ f_arity | t <- ts, f_arity <- functionsWithArity t])

-- Pos(t)
positions :: Term -> [Position]
positions (V _x _) = [[]]
positions (F _f ts _) = ([]: [ i : p | (i, t) <- zip [0..] ts, p <- positions t])

functionPositions :: Term -> [Position]
functionPositions (V _x _) = []
functionPositions (F _f ts _) = ([]: [ i : p | (i, t) <- zip [0..] ts, p <- functionPositions t])

-- subtermAt t p = t|_p
subtermAt :: Term -> Position -> Term
subtermAt t [] = t
subtermAt (V _ _) _ = undefined
subtermAt (F _f ts _) (i : p)
  | i < length ts = subtermAt (ts !! i) p
  | otherwise = undefined

subterms :: Term -> [Term]
subterms t = [subtermAt t p | p <- positions t]

strictSubterms :: Term -> [Term]
strictSubterms t = [subtermAt t p | p <- positions t, p /= []]

isSubtermOf :: Term -> Term -> Bool
isSubtermOf t1 t2 = t1 `elem` (subterms t2)

isStrictSubtermOf :: Term -> Term -> Bool
isStrictSubtermOf t1 t2 = t1 `elem` (strictSubterms t2)

-- replace t u p = t[u]_p
replace :: Term -> Term -> Position -> Term
replace _t u [] = u
replace (V _ _) _ _ = undefined
replace (F f ts _) u (i : p)
  | i < length ts = app f ts'
  | otherwise = undefined
      where ts' = [ if i == j then (replace tj u p) else tj | (j, tj) <- zip [0..] ts]

type Precedence = Map.Map Function Int

showPrec :: FunctionPrinter -> Precedence -> BSB.Builder
showPrec p prec =
  mconcat (List.intersperse (BSB.string7 " > ") [ unsafeLookup f p | (f, _n) <- reverse (List.sortOn snd (Map.toList prec))])

assoc2Prec :: [(Function, Int)] -> Precedence
assoc2Prec as = Map.fromList as

-- `gtPrec > f g` tests `f > g`
gtPrec :: Precedence -> Function -> Function -> Bool
gtPrec prec f g = (unsafeLookup f prec) > (unsafeLookup g prec)

-- FIXME: what is this?
-- only linear polynomials are considered
-- type Algebra = [(String, Int)]
