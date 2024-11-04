{-# LANGUAGE NamedFieldPuns #-}
module KBCompletion where

import Derivation
import qualified E as E
import ES
import Rewriting
import ReductionOrder

-- NOTE: we cannot use list comprehension.
-- delete and simplify must be performed sequentially.
-- NOTE: currently oriented are inter-reduced first, then unoriented.
-- unoriented first is better? (no idea for the moment)
interreduce' :: Int -> ES -> ES -> ReductionOrderParam -> ES -> ES -> ES -> (Int, ES, ES, ES)
interreduce' next_id [] [] _ reduced_oriented reduced_unoriented deleted = (next_id, reduced_oriented, reduced_unoriented, deleted)
interreduce' next_id (e@(E.E { E.eqn_id = i, E.eqn_orientation, E.eqn }) : oriented) unoriented param reduced_oriented reduced_unoriented deleted
  | l' == r' -- joinable?
    = interreduce' next_id oriented unoriented param reduced_oriented reduced_unoriented (e : deleted)
  | null r_steps && null l_steps
    = interreduce' next_id oriented unoriented param (e : reduced_oriented) reduced_unoriented deleted
  | null l_steps
    = let
        e' = E.E { E.eqn = eqn, E.eqn_id = next_id, E.eqn_derivation = d, E.eqn_orientation = eqn_orientation }
      in interreduce' (next_id + 1) oriented unoriented param (e' : reduced_oriented) reduced_unoriented (e : deleted)
  | otherwise
    = let
        e' = E.orient gt (E.E { E.eqn = eqn, E.eqn_id = next_id, E.eqn_derivation = d, E.eqn_orientation = E.Unoriented })
      in if E.oriented e'
          then interreduce' (next_id + 1) oriented unoriented param (e' : reduced_oriented) reduced_unoriented (e : deleted)
          else interreduce' (next_id + 1) oriented unoriented param reduced_oriented (e' : reduced_unoriented) (e : deleted)
  where
    gt = param2ord param
    (l, r) = E.unsafeRule e
    -- compose
    (r_steps, r') = nf (oriented ++ reduced_oriented) (unoriented ++ reduced_unoriented) gt r
    -- collapse
    -- skip encompassment condition check  (only finite runs are considered)
    (l_steps, l') = nf (oriented ++ reduced_oriented) (unoriented ++ reduced_unoriented) gt l
    d = case eqn_orientation of
          E.LR -> Simp { original = i, orig_eqn = eqn, rw_l = l_steps, rw_r = r_steps }
          E.RL -> Simp { original = i, orig_eqn = eqn, rw_l = r_steps, rw_r = l_steps }
          E.Unoriented -> error "interreduce': oriented must be oriented."
    eqn = case eqn_orientation of
            E.LR -> (l', r')
            E.RL -> (r', l')
            E.Unoriented -> error "interreduce': oriented must be oriented."
interreduce' next_id [] (e@(E.E { E.eqn_id = i, E.eqn = (l, r) }) : unoriented) param reduced_oriented reduced_unoriented deleted
  | l' == r' -- joinable?
    = interreduce' next_id [] unoriented param reduced_oriented reduced_unoriented (e : deleted)
  | null r_steps && null l_steps
    = interreduce' next_id [] unoriented param reduced_oriented (e : reduced_unoriented) deleted
  | otherwise
    = let
        e' = E.orient gt (E.E { E.eqn = (l', r'), E.eqn_id = next_id, E.eqn_derivation = d, E.eqn_orientation = E.Unoriented })
      in if E.oriented e'
          then interreduce' (next_id + 1) [] unoriented param (e' : reduced_oriented) reduced_unoriented (e : deleted)
          else interreduce' (next_id + 1) [] unoriented param reduced_oriented (e' : reduced_unoriented) (e : deleted)
  where
    gt = param2ord param
    -- simplify
    -- skip encompassment condition check  (only finite runs are considered)
    (r_steps, r') = nf reduced_oriented (unoriented ++ reduced_unoriented) gt r
    (l_steps, l') = nf reduced_oriented (unoriented ++ reduced_unoriented) gt l
    d = Simp { original = i, orig_eqn = (l, r), rw_l = l_steps, rw_r = r_steps }

-- args: (E, >)
-- performs "simplify" and "delete"
-- returns (next_id, reduced_oriented, reduced_unoriented, deleted)
interreduce :: Int -> ES -> ES -> ReductionOrderParam -> (Int, ES, ES, ES)
interreduce next_id oriented unoriented param
  = interreduce' next_id oriented unoriented param [] [] []
