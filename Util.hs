module Util where

import System.IO (hPutStrLn, stderr)

unsafeLookup :: (Eq a) => a -> [(a, b)] -> b
unsafeLookup a abs'
  | Just b <- lookup a abs' = b
  | otherwise = error "unsafeLookup: the key is not found."

putStdErr :: String -> IO ()
putStdErr s = hPutStrLn stderr s

-- strict order > induced by a list
-- for exmaple, [x1, x2, x3] induces strict order x1 > x2 > x3
-- note that > is not always total
gtByList :: (Eq a) => [a] -> a -> a -> Bool
gtByList [] _ _ = False
gtByList (z : zs) x y
  | x == y = False
  | z == x = elem y zs
  | z == y = False
  | otherwise = gtByList zs x y

-- returns powerset
-- assume no duplicate elements
power :: [a] -> [[a]]
power [] = [[]]
power (x : xs) = power xs ++ [ x : s | s <- power xs]

-- returns all functions from A to B
allFunctions :: [a] -> [b] -> [[(a, b)]]
allFunctions [] _ = [[]]
allFunctions (a : as) bs = [ (a, b) : f | f <- allFunctions as bs, b <- bs]

-- insert x at n-th of list
insertAt :: a -> Int -> [a] -> [a]
insertAt x 0 ys = x : ys
insertAt _ _ [] = error "insertAt: `n <= length ys` must hold"
insertAt x n (y : ys) = y : insertAt x (n-1) ys

-- returns all total strict orders on A
-- i.e. all permutations on A
allTotalStrictOrders :: [a] -> [[a]]
allTotalStrictOrders [] = [[]]
allTotalStrictOrders (a : as) = [ insertAt a i o | o <- allTotalStrictOrders as, i <- [0..n]]
  where n = length as
