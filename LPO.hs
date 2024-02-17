module LPO where
-- Lexicographic Path Order

import Term

-- NOTE: In an experiment
-- bottom-up LPO comparison to avoid worst cases
-- turned out to perform worse...
-- hypotheses:
-- * another bottleneck is hidden (so actually bottom-up comparison is better)
-- * naive recursive implementation indeed outperforms in practice, in particular when only shallow terms are involved?
-- Maedmax uses recursive implementation cf. https://github.com/bytekid/maedmax/blob/df4d5851f628ad4b48e4ac6d2faa460ec0fc5be4/src/termination/lpo.ml#L286

-- `gtLPO > s t` tests `s >_lpo t`.
-- NOTE: gtLPO assume that Precedence is total.
--       this requirement might be inappropriate...
gtLPO :: Precedence -> Term -> Term -> Bool
gtLPO _ (V _ _) _ = False
gtLPO _ s (V x _) = x `elem` (variables s)
gtLPO prec s@(F f ss _) t@(F g ts _)
  | any (\si -> si == t || gtLPO prec si t) ss = True
  | gtPrec prec f g && isGtLPOThanAny s ts = True
  | f == g && isGtLPOThanAny s ts && lexGtLPO ss ts = True
  | otherwise = False
  where isGtLPOThanAny s' ts' = all (\tj -> gtLPO prec s' tj) ts'
        lexGtLPO [] [] = False
        lexGtLPO (s' : ss') (t' : ts')
          | s' == t' = lexGtLPO ss' ts'
          | gtLPO prec s' t' = True
          | otherwise = False
        lexGtLPO _ _ = undefined
