module Termination where
-- termination checker

import GWPO
import LPO
import LPOEncoding
import Term
import TRS
import ReductionOrder

-- TODO: use show  param via module ReductionOrder?

handleLPO :: TRS -> IO ()
handleLPO trs = do
  res <- LPOEncoding.findPrecedence trs
  case res of
    Just prec -> if all (\(l, r) -> gtLPO prec l r) trs
                  then do putStrLn "YES"
                          putStrLn "Termination is shown by LPO:"
                          putStrLn (showPrec prec)
                  else do putStrLn "ERROR"
                          putStrLn "incompatible parameter is found."
                          putStrLn (showPrec prec)
    Nothing -> putStrLn "MAYBE"

handleGWPON :: TRS -> IO ()
handleGWPON trs = do
  res <- findGWPOParamN trs
  case res of
    Just param -> if all (\(l, r) -> gtGWPO param l r) trs
                    then do putStrLn "YES"
                            putStrLn "Termination is shown by GWPO:"
                            putStrLn (showWPOParam param)
                    else do putStrLn "ERROR"
                            putStrLn "incompatible parameter is found."
                            putStrLn (showWPOParam param)
    Nothing -> putStrLn "MAYBE"

handleGWPO01 :: TRS -> IO ()
handleGWPO01 trs = do
  res <- findGWPOParam01 trs
  case res of
    Just param -> if all (\(l, r) -> gtGWPO param l r) trs
                    then do putStrLn "YES"
                            putStrLn "Termination is shown by GWPO:"
                            putStrLn (showWPOParam param)
                    else do putStrLn "ERROR"
                            putStrLn "incompatible parameter is found."
                            putStrLn (showWPOParam param)
    Nothing -> putStrLn "MAYBE"

handleGWPO :: TRS -> IO ()
handleGWPO trs = do
  res <- findGWPOParam trs
  case res of
    Just param -> if all (\(l, r) -> gtGWPO param l r) trs
                    then do putStrLn "YES"
                            putStrLn "Termination is shown by GWPO:"
                            putStrLn (showWPOParam param)
                    else do putStrLn "ERROR"
                            putStrLn "incompatible parameter is found."
                            putStrLn (showWPOParam param)
    Nothing -> putStrLn "MAYBE"

handleWPO :: TRS -> IO ()
handleWPO trs = do
  res <- findWPOParam trs
  case res of
    Just param -> if all (\(l, r) -> gtWPO param l r) trs
                    then do putStrLn "YES"
                            putStrLn "Termination is shown by WPO:"
                            putStrLn (showWPOParam param)
                    else do putStrLn "ERROR"
                            putStrLn "incompatible parameter is found."
                            putStrLn (showWPOParam param)
    Nothing -> putStrLn "MAYBE"

handleLPOviaGWPO :: TRS -> IO ()
handleLPOviaGWPO trs = do
  res <- GWPO.findLPOParam trs
  case res of
    Just prec -> if all (\(l, r) -> gtLPO prec l r) trs
                    then do putStrLn "YES"
                            putStrLn "Termination is shown by GWPO:"
                            putStrLn (showPrec prec)
                            putStrLn "algebra: max interpretation over N"
                    else do putStrLn "ERROR"
                            putStrLn "incompatible precedence is found."
                            putStrLn (showPrec prec)
    Nothing -> putStrLn "MAYBE"

terminationChecker :: ReductionOrder.Class -> TRS -> IO ()
terminationChecker LPO trs = handleLPO trs
terminationChecker GWPO trs = handleGWPO trs
terminationChecker GWPON trs = handleGWPON trs
terminationChecker GWPO01 trs = handleGWPO01 trs
terminationChecker WPO trs = handleWPO trs
terminationChecker LPOviaGWPO trs = handleLPOviaGWPO trs
