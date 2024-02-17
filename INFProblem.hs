module INFProblem where

import CES

data INFProblem = SemiEquational (CES, PES) | Join (CES, PES) | Oriented (CES, PES)
-- the second argument of the tuple is CONDITION (existential closure of the conjunction of conditions)
