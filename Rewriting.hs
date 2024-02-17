module Rewriting (nf) where

import qualified E as E
import ES
import Substitution as S
import Term

-- innermost rewriting

data MarkedTerm = NF Term
                | Active Function [MarkedTerm]

-- special subsitute for innermost rewriting
substitute :: Term -> Subst -> MarkedTerm
substitute t@(V _ _) sigma = NF (S.substitute t sigma)
substitute (F f ts _) sigma = Active f [ Rewriting.substitute t sigma | t <- ts ]

activate :: Term -> MarkedTerm
activate t@(V _ _) = NF t
activate (F f ts _) = Active f [ activate t | t <- ts ]

-- give priority to oriented.
rewriteAtRoot :: ES -> ES -> TermOrder -> Term -> (Maybe Int, MarkedTerm)
rewriteAtRoot [] [] _gt t = (Nothing, NF t)
rewriteAtRoot (e@(E.E { E.eqn_id = i }) : oriented) unoriented gt t
  | E.oriented e, Just (l, r) <- E.rule e, Just sig <- match l t = (Just i, Rewriting.substitute r sig)
  | E.oriented e = rewriteAtRoot oriented unoriented gt t -- oriented but failed pattern-matching
  | otherwise = error "rewriteAtRoot: oriented is not oriented."
rewriteAtRoot [] (E.E { E.eqn = (l, r), E.eqn_id = i, E.eqn_orientation = E.Unoriented } : unoriented) gt t
  | Just sig <- match l t,  (S.substitute l sig) `gt` (S.substitute r sig)
    = (Just i, Rewriting.substitute r sig)
  | Just sig <- match r t, (S.substitute r sig) `gt` (S.substitute l sig)
    = (Just i, Rewriting.substitute l sig)
  | otherwise = rewriteAtRoot [] unoriented gt t
rewriteAtRoot [] (e : _) _gt _t
  | E.oriented e = error "rewriteAtRoot: unoriented is oriented."
rewriteAtRoot _ _ _ _ = error "rewriteAtRoot"

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

nfArgs' :: ES -> ES -> TermOrder -> [MarkedTerm] -> [([Int], Term)] -> ([Int], [Term])
-- NOTE: args are parallel. So we don't need to reverse id_acc.
nfArgs' _ _ _ [] id_term_acc = (Prelude.concat id_acc, reverse term_acc)
  where
    id_acc = map fst id_term_acc
    term_acc = map snd id_term_acc
nfArgs' oriented unoriented gt (t : ts) id_term_acc
  = nfArgs' oriented unoriented gt ts (nf' oriented unoriented gt t : id_term_acc)

nfArgs :: ES -> ES -> TermOrder -> [MarkedTerm] -> ([Int], [Term])
nfArgs oriented unoriented gt ts = nfArgs' oriented unoriented gt ts []

nf'' :: ES -> ES -> TermOrder -> [Int] -> MarkedTerm -> ([Int], Term)
nf'' _ _ _ id_acc (NF t) = (id_acc, t)
nf'' oriented unoriented gt id_acc (Active f ts) =
  case rewriteAtRoot oriented unoriented gt (app f nf_args) of
    (Just i, mt)    -> nf'' oriented unoriented gt (i : arg_ids ++ id_acc) mt
    (Nothing, NF t) -> (arg_ids ++ id_acc, t)
    _               -> error "nf"
  where (arg_ids, nf_args) = nfArgs oriented unoriented gt ts

nf' :: ES -> ES -> TermOrder -> MarkedTerm -> ([Int], Term)
nf' oriented unoriented gt t = nf'' oriented unoriented gt [] t

-- returns (eqn_ids, t)
-- eqn_ids are sorted from earlier to later in innermost rewriting.
-- NOTE: oriented and unoriented are separated in advance to avoid the cost of sorting.
nf :: ES -> ES -> TermOrder -> Term -> ([Int], Term)
nf oriented unoriented gt t = (reverse ids, t') -- NOTE: reverse must be done only once here.
  where (ids, t') = nf' oriented unoriented gt (activate t)
