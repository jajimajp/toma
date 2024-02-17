module GWPO where
-- GWPO
-- interpretation: weakly monotone algebra on N with coefficients

import Data.List
import SMT
import Term
import TRS
import Z3

-- (algebra, precedence)
type WPOParam = (Algebra, Precedence)

showWPOParam :: WPOParam -> String
showWPOParam (algebra, prec) =
  "precedence: " ++ (showPrec prec) ++ "\n" ++
  "algebra:\n" ++
  (intercalate "\n" [ f ++ " = " ++ show n | (f, n) <- sort algebra])

-- internal name of coefficients.
ith_coef :: String -> Int -> String
ith_coef f n = "coef_" ++ f ++ "_" ++ (show n)

-- internal name of precedence.
prec_name :: String -> String
prec_name f = f -- Don't worry, Z3 module does proper renaming. 

-- Assume that every interpretation function f_A is defined as:
--
--    f_A(x_1,...,x_n) = a_0 + a_1 x_1 + ... + a_n x_n
--
-- coefficient t x returns the coefficient of x in the interpretation of t.
-- E.g., coefficient (F "a" [F "s" [V "x"], V "x"]) "x" returns
-- the expression that represents (+ (* a1 s1) a2).
coefficientExp ::  Term -> String -> Exp
coefficientExp (V x) y
  | x == y = Val 1
  | otherwise = Val 0
coefficientExp (F f ts) y =
  Plus [ Times [(Var (ith_coef f i)), (coefficientExp t y)] | (i, t) <- zip [1..] ts ]

coefficientExp01' :: Term -> String -> [[String]]
coefficientExp01' (V x) y
  | x == y = [[]]
  | otherwise = []
coefficientExp01' (F f ts) y =
  [ (ith_coef f i) : cs | (i, t) <- zip [1..] ts, cs <- coefficientExp01' t y ]

coefficientExp01 :: Term -> String -> Exp
coefficientExp01 t x = Plus [ Ite (AndE [Var c | c <- coefs]) (Val 1) (Val 0) | coefs <- coefficientExp01' t x ]

-- >>> coefficientExp01 (F "f" [F "g" [F "f" [V "x"]]]) "x"
-- (ite (and coef_f_1 coef_g_1 coef_f_1) 1 0)
--

-- constraint t returns the constant part of the interpretation of t.
-- E.g., constant (F "a" [F "s" [V "x"], V "x"]) returns
-- the expression that represents (+ a0 (* a1 s0)).
constantExp :: Term -> Exp
constantExp (V _x) = Val 0
constantExp (F f ts) =
  Plus ((Var (ith_coef f 0)) : [ Times [ Var (ith_coef f i), constantExp t] | (i, t) <- zip [1..] ts])

constantExp01' :: Term -> [([String], String)]
constantExp01' (V _) = []
constantExp01' (F f ts) =
  ([], ith_coef f 0) : [ ((ith_coef f i) : cs, c0) | (i, t) <- zip [1..] ts, (cs, c0) <- constantExp01' t ]

constantExp01 :: Term -> Exp
constantExp01 t =
  Plus [ Ite (AndE [Var c | c <- coefs]) (Var c0) (Val 0) | (coefs, c0) <- constantExp01' t ]

-- NOTE: this should be called interpretation?
data GWPOMode = N -- interpretation on natural numbers
              | ZeroOne -- interpretation on 0-1 coefficients
              | Max -- LPO simulation.
-- NOTE: on LPO simulation
-- f_A(x_1, ..., x_n) = max{x_1, ..., x_n} for all f in F.
-- In particular c_A = 0 for all constants c.

-- >_A
encodeAlgebraGtN :: Term -> Term -> Formula
encodeAlgebraGtN l r =
  andFormula ((Gt (constantExp l) (constantExp r)) : [ Geq (coefficientExp l x) (coefficientExp r x) | x <- vars ])
  where
    vars = Term.variables l

encodeAlgebraGt01 :: Term -> Term -> Formula
encodeAlgebraGt01 l r =
  andFormula ((Gt (constantExp01 l) (constantExp01 r)) : [ Geq (coefficientExp01 l x) (coefficientExp01 r x) | x <- vars ])
  where
    vars = Term.variables l

encodeAlgebraGt :: GWPOMode -> Term -> Term -> Formula
encodeAlgebraGt N s t = encodeAlgebraGtN s t
encodeAlgebraGt ZeroOne s t = encodeAlgebraGt01 s t
encodeAlgebraGt Max _s _t = falseFormula

-- >=_A
encodeAlgebraGeqN :: Term -> Term -> Formula
encodeAlgebraGeqN l r =
  andFormula ((Geq (constantExp l) (constantExp r)) : [ Geq (coefficientExp l x) (coefficientExp r x) | x <- vars ])
  where
    vars = Term.variables l

encodeAlgebraGeq01 :: Term -> Term -> Formula
encodeAlgebraGeq01 l r =
  andFormula ((Geq (constantExp01 l) (constantExp01 r)) : [ Geq (coefficientExp01 l x) (coefficientExp01 r x) | x <- vars ])
  where
    vars = Term.variables l

encodeAlgebraGeq :: GWPOMode -> Term -> Term -> Formula
encodeAlgebraGeq N s t = encodeAlgebraGeqN s t
encodeAlgebraGeq ZeroOne s t = encodeAlgebraGeq01 s t
encodeAlgebraGeq Max s t =
  if null (Term.variables t \\  Term.variables s) 
    then trueFormula
    else falseFormula

encodeGWPO :: GWPOMode -> Term -> Term -> Formula
encodeGWPO m s t = andFormula [ encodeAlgebraGeq m s t, encodeWPO m s t]

encodeWPO :: GWPOMode -> Term -> Term -> Formula
encodeWPO m l r =
  orFormula [encodeWPOSubterm m l r, encodeWPOAlgebra m l r, encodeWPOLex m l r, encodeWPOPrecedence m l r]

encodeWPOSubterm :: GWPOMode -> Term -> Term -> Formula
encodeWPOSubterm m (F _f ss) r
  | any (r ==) ss = trueFormula
  | otherwise = orFormula [ encodeWPO m si r | si <- ss ]
encodeWPOSubterm _ _ _ = falseFormula

encodeWPOAlgebra :: GWPOMode -> Term -> Term -> Formula
encodeWPOAlgebra m s@(F _f _ss) t@(F _g ts) =
  andFormula (encodeAlgebraGt m s t : [ encodeWPO m s tj | tj <- ts ])
encodeWPOAlgebra _ _ _ = falseFormula

encodeWPOLex' :: GWPOMode -> [Term] -> [Term] -> Formula
encodeWPOLex' _ [] [] = falseFormula
encodeWPOLex' _ _ [] = error "encodeWPOLex: ill-formed term is found."
encodeWPOLex' _ [] _ = error "encodeWPOLex: ill-formed term is found."
encodeWPOLex' m (si : ss) (ti : ts)
  | si == ti = encodeWPOLex' m ss ts
  | otherwise = encodeWPO m si ti

encodeWPOLex :: GWPOMode -> Term -> Term -> Formula
encodeWPOLex m s@(F f ss) t@(F g ts)
  | f == g = andFormula (encodeAlgebraGeq m s t : encodeWPOLex' m ss ts : [ encodeWPO m s tj | tj <- ts ])
encodeWPOLex _ _ _ = falseFormula

encodeGtPrecedence :: String -> String -> Formula
encodeGtPrecedence f g = Gt (Var (prec_name f)) (Var (prec_name g))

encodeWPOPrecedence :: GWPOMode -> Term -> Term -> Formula
encodeWPOPrecedence m s@(F f _ss) t@(F g ts) = 
  andFormula (encodeAlgebraGeq m s t :  encodeGtPrecedence f g : [ encodeWPO m s tj | tj <- ts ])
encodeWPOPrecedence _ _ _ = falseFormula

-- unsafe version lookup
lookup' :: (Eq a, Show a) => a -> [(a, b)] -> b
lookup' x xys =
  case (lookup x xys) of
    Just y -> y
    Nothing -> error ("lookup': key \"" ++ show x ++ "\" is not found.")

buildWPOParams :: SMTOutput  -> [String] -> [String] -> Maybe WPOParam
buildWPOParams (Just params) z3Vars_coef z3Vars_prec = Just (algebra, prec)
  where
    algebra = [ (v, smtValueUnsafeCastInt (lookup' v params)) | v <- z3Vars_coef ]
    prec = assoc2Prec [ (v, smtValueUnsafeCastInt sn) | v <- z3Vars_prec, let sn = lookup' v params ]
buildWPOParams Nothing _ _ = Nothing

findGWPOParamN :: TRS -> IO (Maybe WPOParam)
findGWPOParamN trs = do
  z3output <- z3 "toma" input
  return (buildWPOParams z3output z3Vars_coef z3Vars_prec)
  where
    fs = functionsInTRSWithArity trs
    z3Vars_coef = [ ith_coef f i | (f, n) <- fs, i <- [0..n] ]
    z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
    z3Vars = z3Vars_coef ++ z3Vars_prec
    orientation_formula = andFormula [ encodeGWPO N l r | (l, r) <- trs ]
    input = [ DeclareInt v | v <- z3Vars ] ++
            -- weak monotonicity of algebra
            [ Assert (Geq (Var v) (Val 0)) | v <- z3Vars ] ++
            -- totality of precedence
            [ Assert (Distinct [ Var v | v <- z3Vars_prec ]) ]  ++
            [ Assert orientation_formula, CheckSat, GetValue z3Vars ]

findGWPOParam01 :: TRS -> IO (Maybe WPOParam)
findGWPOParam01 trs = do
  z3output <-  z3 "toma" input
  return (buildWPOParams z3output z3Vars_coef z3Vars_prec)
  where
    fs = functionsInTRSWithArity trs
    z3Vars_coef1n = [ ith_coef f i | (f, n) <- fs, i <- [1..n] ]
    z3Vars_coef0 = [ ith_coef f 0 | (f, _n) <- fs ]
    z3Vars_coef = z3Vars_coef1n ++ z3Vars_coef0
    z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
    z3Vars = z3Vars_coef ++ z3Vars_prec
    orientation_formula = andFormula [ encodeGWPO ZeroOne l r | (l, r) <- trs ]
    input = [ DeclareInt v | v <- z3Vars_coef0 ++ z3Vars_prec ] ++
            [ DeclareBool v | v <- z3Vars_coef1n ] ++
            -- weak monotonicity of algebra
            [ Assert (Geq (Var v) (Val 0)) | v <- z3Vars_coef0 ++ z3Vars_prec ] ++
            -- totality of precedence
            [ Assert (Distinct [ Var v | v <- z3Vars_prec ]) ]  ++
            [ Assert orientation_formula, CheckSat, GetValue z3Vars ]

-- first tries 0-1 coefficients, and then coefficients over N.
findGWPOParam :: TRS -> IO (Maybe WPOParam)
findGWPOParam trs = do
  res01 <- findGWPOParam01 trs
  case res01 of
    Just p -> return (Just p)
    Nothing -> findGWPOParamN trs

findWPOParam :: TRS -> IO (Maybe WPOParam)
findWPOParam trs = do
  z3output <- z3 "toma" input
  return (buildWPOParams z3output z3Vars_coef z3Vars_prec)
  where
    fs = functionsInTRSWithArity trs
    z3Vars_coef1n = [ ith_coef f i | (f, n) <- fs, i <- [1..n] ]
    z3Vars_coef0 = [ ith_coef f 0 | (f, _n) <- fs ]
    z3Vars_coef = z3Vars_coef0 ++ z3Vars_coef1n
    z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
    z3Vars = z3Vars_coef ++ z3Vars_prec
    orientation_formula = andFormula [ encodeWPO N l r | (l, r) <- trs ]
    input = [ DeclareInt v | v <- z3Vars ] ++
            -- strictly monotone algebra
            -- strict monotonicity implies weak monotonicity and simplicity.
            [ Assert (Gt (Var v) (Val 0)) | v <- z3Vars_coef1n ] ++
            -- constant parts must not be negative for weak monotonicity and simplicity.
            [ Assert (Geq (Var v) (Val 0)) | v <- z3Vars_coef0 ] ++
            -- totality of precedence
            [ Assert (Distinct [ Var v | v <- z3Vars_prec ]) ]  ++
            [ Assert orientation_formula, CheckSat, GetValue z3Vars ]

buildLPOParams :: SMTOutput  -> [String] -> Maybe Precedence
buildLPOParams (Just params) z3Vars_prec = Just prec
  where
    prec = assoc2Prec [ (v, smtValueUnsafeCastInt sn) | v <- z3Vars_prec, let sn = lookup' v params ]
buildLPOParams Nothing _ = Nothing

findLPOParam :: TRS -> IO (Maybe Precedence)
findLPOParam trs = do
  z3output <- z3 "toma" input
  return (buildLPOParams z3output z3Vars_prec)
  where
    fs = functionsInTRSWithArity trs
    z3Vars_prec = [ prec_name f | (f, _n) <- fs ]
    orientation_formula = andFormula [ encodeWPO Max l r | (l, r) <- trs ]
    input = [ DeclareInt v | v <- z3Vars_prec ] ++
            -- totality of precedence
            [ Assert (Distinct [ Var v | v <- z3Vars_prec ]) ]  ++
            [ Assert orientation_formula, CheckSat, GetValue z3Vars_prec ]

-- run-time GWPO --

coefficient ::  Algebra -> Term -> String -> Int
coefficient _alg (V x) y
  | x == y = 1
  | otherwise = 0
coefficient alg (F f ts) y =
  sum [ (lookup' (ith_coef f i) alg) * (coefficient alg t y)  | (i, t) <- zip [1..] ts ]

constant :: Algebra -> Term -> Int
constant _alg (V _x) = 0
constant alg (F f ts) =
  (sum [ (lookup' (ith_coef f i) alg) * (GWPO.constant alg t)  | (i, t) <- zip [1..] ts ]) + (lookup' (ith_coef f 0) alg)

-- s >=_A t
geqAlg :: Algebra -> Term -> Term -> Bool
geqAlg a s t =
  (const_s >= const_t) && all (\x -> (coefficient a s x) >= (coefficient a t x)) vars
  where
    const_s = GWPO.constant a s
    const_t = GWPO.constant a t
    vars = nub ((Term.variables s) ++ (Term.variables t))

-- s >_A t
gtAlg :: Algebra -> Term -> Term -> Bool
gtAlg a s t =
  (const_s > const_t) && all (\x -> (coefficient a s x) >= (coefficient a t x)) vars
  where
    const_s = GWPO.constant a s
    const_t = GWPO.constant a t
    vars = nub ((Term.variables s) ++ (Term.variables t))

-- s >_wpo t
gtWPO :: WPOParam -> Term -> Term -> Bool
gtWPO param s t =
  gtWPOSubterm param s t || gtWPOAlgebra param s t || gtWPOLex param s t || gtWPOPrecedence param s t

gtWPOSubterm :: WPOParam -> Term -> Term -> Bool
gtWPOSubterm param (F _f ss) t
  | any (t ==) ss = True
  | otherwise = any (\s -> gtWPO param s t) ss
gtWPOSubterm _ _ _ = False

gtWPOAlgebra :: WPOParam -> Term -> Term -> Bool
gtWPOAlgebra param@(a, _) s@(F _f _ss) t@(F _g ts) =
  gtAlg a s t && all (\tj -> gtWPO param s tj) ts
gtWPOAlgebra _ _ _ = False

gtWPOLex' :: WPOParam -> [Term] -> [Term] -> Bool
gtWPOLex' _param _ [] = False
gtWPOLex' _param [] _ = False
gtWPOLex' param (si : ss) (ti : ts)
  | si == ti = gtWPOLex' param ss ts
  | otherwise = gtWPO param si ti
  
gtWPOLex :: WPOParam -> Term -> Term -> Bool
gtWPOLex param@(a, _) s@(F f ss) t@(F g ts) =
  f == g && geqAlg a s t && gtWPOLex' param ss ts && all (\tj -> gtWPO param s tj) ts
gtWPOLex _ _ _ = False

gtWPOPrecedence :: WPOParam -> Term -> Term -> Bool
gtWPOPrecedence param@(a, prec) s@(F f _ss) t@(F g ts) =
  geqAlg a s t && gtPrec prec (prec_name f) (prec_name g) && all (\tj -> gtWPO param s tj) ts
gtWPOPrecedence _ _ _ = False

gtGWPO :: WPOParam -> Term -> Term -> Bool
gtGWPO param@(alg, _) s t = geqAlg alg s t && gtWPO param s t
