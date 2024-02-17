module TRS where

import Term
import PES

type Rule = TermPair
type TRS = [Rule]
-- index of arguments starts from 1.
type ReplacementMap = [(String, [Int])]

functionsInTRS :: TRS -> [Function]
functionsInTRS = functionsInES

-- NOTE: this function assumes that arity is consistent.
functionsInTRSWithArity :: TRS -> [(Function, Int)]
functionsInTRSWithArity = functionsInESWithArity 
