module CES where

import Data.List
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import ParserTerm as PT
import qualified Data.ByteString.Builder as BSB

-- NOTE: temporaly copied from CES.hs
type PE = PT.TermPair
type PES = [PE]
type CEquation = ([PE], PE)
type CES = [CEquation]

showCondition :: PES -> BSB.Builder
showCondition es = 
  mconcat (intersperse (BSB.string7 ", ") [ showTerm s <> (BSB.string7 " == ") <> showTerm t | (s, t) <- es ])

showCES :: CES -> BSB.Builder
showCES ces =
  mconcat (intersperse (BSB.string7 "\n") [ showTerm l <> (BSB.string7 " -> ") <> showTerm r <> (BSB.string7 "<==") <> showCondition es  | (es, (l, r)) <- ces ])
