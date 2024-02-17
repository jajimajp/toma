module Substitution where

import Term

type Subst = [(Term.Var, Term)]

-- substitute t σ= tσ
substitute :: Term -> Subst -> Term
substitute t@(V x _) s
  | Just u <- lookup x s = u
  | otherwise = t
substitute (F f ts _) s = app f [ substitute t s | t <- ts ]

-- compose s1 s2 = s1 s2
compose :: Subst -> Subst -> Subst
compose s1 s2 = [(x, substitute t s2) | (x,t) <- s1] ++ s2'
  where v1 = [x | (x, _) <- s1 ]
        s2' = [(x, t) | (x, t) <- s2, not $ elem x v1]

isRenaming :: Subst -> Bool
isRenaming [] = True
isRenaming ((_ , V _ _) : subst) = isRenaming subst
isRenaming _ = False

-- match l t = Just σ, if lσ = t for some σ 
-- match l t = Nothing, otherwise
match :: Term -> Term -> Maybe Subst
match s t = match' [] [(s, t)]

match' :: Subst -> [(Term,Term)] -> Maybe Subst
match' sigma [] = Just sigma
match' sigma ((V x _, t) : ps)
     | Nothing <- m        = match' ((x, t) : sigma) ps
     | Just u <- m, t == u = match' sigma ps
     | otherwise           = Nothing
         where m = lookup x sigma
match' sigma ((F f ss _, F g ts _) : ps)
   | f == g    = match' sigma (zip ss ts ++ ps)
match' _ _ = Nothing

-- `subsume s t` tests if `s subsumes t`
-- e.g. `subsume (0 + X) s(0 + s(X)) ` is true
subsume :: Term -> Term -> Bool
subsume s t = case match s t of
                Just _  -> True
                Nothing -> False

unify' :: [(Term,Term)] -> Maybe Subst
unify' [] = Just []
unify' ((F f ss _, F g ts _) : sts)
  | f == g = unify' ((zip ss ts) ++ sts)
  | otherwise = Nothing
unify' ((v@(V x _), t) : sts)
  | t == v = unify' sts
  | elem x (variables t) = Nothing
  | otherwise = do subst <- unify' sts'
                   return $ compose sgm subst
  where
    sgm = [(x,t)]
    sts' = [(substitute s sgm, substitute t' sgm) | (s, t') <- sts]
unify' ((s, V x _) : sts) = unify' ((var x, s) : sts)

-- returns mgu(s, t) if exists
unify :: Term -> Term -> Maybe Subst
unify s t = unify' [(s,t)]

-- `encompass s t` tests if `s encompasses t`
-- e.g. `encompass s(0 + s(X)) (0 + X)` is true
encompass :: Term -> Term -> Bool
encompass s t = any (subsume t) (subterms s)
