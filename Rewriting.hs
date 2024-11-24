module Rewriting (nf) where

import qualified E
import ES
import Substitution as S
import Term ( app, Function, Term(..), TermOrder, showTerm, Position )
import qualified Data.Map as Map
import qualified Data.ByteString.Builder as BSB
import Derivation
import qualified Data.Bifunctor

-- innermost rewriting

data MarkedTerm = NF Term
                | Active Function [MarkedTerm]


-- replace nth(0-indexed) argument of a term.
replace :: Term -> Int -> Term -> Term
replace (V _ _) _ _ = error "replace: variable"
replace (F f ts s) n t = F f (replace' ts n t) s
  where replace' [] _ _ = error "replace: out of bound"
        replace' (t' : ts') 0 t = t : ts'
        replace' (t' : ts') n t = t' : replace' ts' (n - 1) t

-- special subsitute for innermost rewriting
substitute :: Term -> Subst -> MarkedTerm
substitute t@(V _ _) sigma = NF (S.substitute t sigma)
substitute (F f ts _) sigma = Active f [ Rewriting.substitute t sigma | t <- ts ]

activate :: Term -> MarkedTerm
activate t@(V _ _) = NF t
activate (F f ts _) = Active f [ activate t | t <- ts ]

termOfMarkedTerm :: MarkedTerm -> Term
termOfMarkedTerm (NF t) = t
termOfMarkedTerm (Active f ts) = app f (map termOfMarkedTerm ts)

-- give priority to oriented.
rewriteAtRoot :: ES -> ES -> TermOrder -> Term -> (Maybe TermRewriteStep, MarkedTerm)
rewriteAtRoot [] [] _gt t = (Nothing, NF t)
rewriteAtRoot (e@(E.E { E.eqn_id = i }) : oriented) unoriented gt t
  | E.oriented e, Just (l, r) <- E.rule e, Just sig <- match l t =
    (Just (i, []), Rewriting.substitute r sig)
  | E.oriented e = rewriteAtRoot oriented unoriented gt t -- oriented but failed pattern-matching
  | otherwise = error "rewriteAtRoot: oriented is not oriented."
rewriteAtRoot [] (E.E { E.eqn = (l, r), E.eqn_id = i, E.eqn_orientation = E.Unoriented } : unoriented) gt t
  | Just sig <- match l t,  S.substitute l sig `gt` S.substitute r sig
    = (Just (i, []), Rewriting.substitute r sig)
  | Just sig <- match r t, S.substitute r sig `gt` S.substitute l sig
    = (Just (i, []), Rewriting.substitute l sig)
  | otherwise = rewriteAtRoot [] unoriented gt t
rewriteAtRoot [] (e : _) _gt _t
  | E.oriented e = error "rewriteAtRoot: unoriented is oriented."
rewriteAtRoot __ _ _ _ = error "rewriteAtRoot"

-- NOTE:
-- [Int] is the eqn_ids of E used to nf a term.
-- Almost every nf related function maitains eqn_ids in a way that
-- rules are sorted from (used) later to earlier in innermost rewriting,
-- for efficiency and ease.
-- For example,
-- Let E = { (a): 0 + y -> y, (b): s(x) + y -> s(x+y) }.
-- innermost rewriting from (s(0) + 0) + 0 is:
-- (s(0) + 0) + 0 -b-> s(0+0) + 0 -a-> s(0) + 0 -b-> s(0 + 0) -a-> s(0). 
-- `nf' E > ((s(0) + 0) + 0)`
-- returns [(a) ,(b), (a), (b)]. Note that rewriting with (b) comes last.
-- In contrast, `nf E > ((s(0) + 0) + 0)`
-- returns [(b), (a), (b), (a)]. This should be what users expect.
-- TODO: add more clear example here

nfArgs' :: ES -> ES -> TermOrder -> [MarkedTerm] -> [([TermRewriteStep], Term)] -> [([TermRewriteStep], Term)]
nfArgs' _ _ _ [] id_term_acc = reverse id_term_acc
nfArgs' oriented unoriented gt (t : ts) id_term_acc
  = nfArgs' oriented unoriented gt ts (nf' oriented unoriented gt t : id_term_acc)

nfArgs :: ES -> ES -> TermOrder -> [MarkedTerm] -> [([TermRewriteStep], Term)]
nfArgs oriented unoriented gt ts = nfArgs' oriented unoriented gt ts []

-- [wrapSteps' t n arg_steps acc] は t の TermRewriteStep を算出する。
-- n は 0-indexed
wrapSteps' :: Term -> Int -> [([TermRewriteStep], Term)] -> [[TermRewriteStep]] -> ([TermRewriteStep], Term)
wrapSteps' t n [] acc =
  if length acc == n
  then (concat (reverse acc), t)
  else error "wrapSteps': out of bound"
wrapSteps' t n ((steps, s) : ss) acc = wrapSteps' t' (n + 1) ss (steps' : acc)
  where t' = replace t n s
        steps' = map (Data.Bifunctor.second (n :)) steps

-- [wrapSteps t arg_steps] は t の TermRewriteStep を算出する。
-- t が V のとき、arg_steps は空でなければならない。
-- t が F のとき、t の subterm それぞれが arg_steps の書換によって変わっていくので、これらを順に適用して t の steps を組み立てる。
wrapSteps :: Term -> [([TermRewriteStep], Term)] -> ([TermRewriteStep], Term)
wrapSteps (V _ _) _ = error "wrapSteps: variable"
wrapSteps (F f ts s) arg_steps = (steps, f')
  -- TODO: the 2nd return value was meant to be f' but it does not work (nf does not terminate).
  where (steps, _) = wrapSteps' (F f ts s) 0 arg_steps []
        f' = app f (map snd arg_steps)


nf'' :: ES -> ES -> TermOrder -> [TermRewriteStep] -> MarkedTerm -> ([TermRewriteStep], Term)
nf'' _ _ _ id_acc (NF t) = (id_acc, t)
nf'' oriented unoriented gt steps_acc (Active f ts) =
  case rewriteAtRoot oriented unoriented gt new_t of
    (Just step, mt) -> nf'' oriented unoriented gt (steps_acc ++ steps ++ [step]) mt
    (Nothing, NF t) -> (steps_acc ++ steps, t)
    _               -> error "nf"
  -- where rewritten = nfArgs oriented unoriented gt ts
  --       (steps', nf_args) = unzip rewritten
  --       steps = concat steps'
  where rewritten = nfArgs oriented unoriented gt ts
        -- ts' = map snd rewritten
        (steps, new_t) = wrapSteps (app f (map termOfMarkedTerm ts)) rewritten
        -- new_t = app f ts'

nf' :: ES -> ES -> TermOrder -> MarkedTerm -> ([TermRewriteStep], Term)
nf' oriented unoriented gt = nf'' oriented unoriented gt []

-- returns (eqn_ids, t)
-- eqn_ids are sorted from earlier to later in innermost rewriting.
-- NOTE: oriented and unoriented are separated in advance to avoid the cost of sorting.
nf :: ES -> ES -> TermOrder -> Term -> ([TermRewriteStep], Term)
nf oriented unoriented gt t = (steps, t')
  where (steps, t') = nf' oriented unoriented gt (activate t)
