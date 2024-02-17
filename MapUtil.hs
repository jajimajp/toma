module MapUtil where

import qualified Data.Map as Map

unsafeLookup :: Ord k => k -> Map.Map k b -> b
unsafeLookup x m
  | Just y <- Map.lookup x m = y
  | otherwise = error "the key is not found."
