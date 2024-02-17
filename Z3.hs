module Z3 (z3, solve) where

import System.Directory
import System.Process
import System.IO
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import SMT
import SMTParser

renameSMTOutput :: SMTOutput -> [(String, String)] -> SMTOutput
renameSMTOutput Nothing _ = Nothing
renameSMTOutput (Just svs) dic = Just [ (SMT.rename s dic, v)  | (s, v) <- svs ]

solve :: String -> String -> SMTInput -> IO SMTOutput
solve toolPath tmpFileName input = do
  (filepath, tmph) <- openTempFile "/tmp/" (tmpFileName ++ ".smt2")
  -- https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Builder.html#v:hPutBuilder
  -- > It is recommended that the Handle is set to binary and BlockBuffering mode. See hSetBinaryMode and hSetBuffering.
  hSetBinaryMode tmph True
  hSetBuffering tmph (BlockBuffering Nothing) -- is Nothing ok?
  BSB.hPutBuilder tmph (showSMTInput renamedInput)
  hFlush tmph -- don't forget flush!
  hClose tmph
  (_, Just hout, _, _) <-
      createProcess (proc toolPath [filepath]){ std_out = CreatePipe }
  z3Output <- BS.hGetContents hout
  -- NOTE: hGetContents uses lazy IO.
  -- Don't close `hout` before `parse` finishes.
  case parse parseSMTOutput "z3Output" (C.unpack z3Output) of
      Right r -> do
        hClose hout
        removeFile filepath
        return (renameSMTOutput r rdic)
      Left e -> do
        hClose hout
        removeFile filepath
        error (show e)
  where vars = variables input
        -- rename variable names for Z3
        -- for example, Z3 does not accept constant name "true" "false"
        dic = [ (v, "__f" ++ show i) | (v, i) <- zip vars ([0..] :: [Int]) ]
        rdic = [ (y, x) | (x, y) <- dic ]
        renamedInput = renameSMTInput input dic

z3 :: String -> SMTInput -> IO SMTOutput
z3 tmpFileName input = solve "z3" tmpFileName input
