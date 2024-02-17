module TRSParser (readTRSFile) where

import Data.List
-- import Term
import ParserTerm
import TRS
import Substitution
import Text.ParserCombinators.Parsec

variables :: ParserTerm.Term -> [ParserTerm.Var]
variables (V x)    = [x]
variables (F _ ts) = nub [ x | t <- ts, x <- TRSParser.variables t ]

variablesInTRS :: TRS -> [ParserTerm.Var]
variablesInTRS trs =
  nub [ x | (l, r) <- trs, t <- [l, r], x <- TRSParser.variables t ]

substitute :: Term -> Subst -> Term
substitute (V x) sigma
    | Just t <- lookup x sigma = t
    | otherwise                = V x
substitute (F f ts) sigma      = F f [ TRSParser.substitute t sigma | t <- ts ]

substituteTRS :: TRS -> Subst -> TRS
substituteTRS trs sigma =
    [ (TRSParser.substitute l sigma, TRSParser.substitute r sigma) 
    | (l, r) <- trs ]

convert :: [String] -> TRS -> TRS
convert xs trs = substituteTRS trs sigma
    where sigma = [ (x, F x []) | x <- TRSParser.variablesInTRS trs, not (elem x xs) ]

-- Scanners.

identifier :: Parser String
identifier = do
  spaces
  x <- many1 (noneOf "(), \t\r\n")
  spaces
  return x

keyword :: String -> Parser ()
keyword s = do
  spaces
  _ <- string s
  spaces
  return ()

nat :: Parser Int
nat = do
  spaces
  s <- many1 digit
  spaces
  return (read s)

-- Parsing functions.
            
parseTerm :: Parser Term
parseTerm = try parseFunction <|> parseVariable

parseVariable :: Parser Term
parseVariable = do
  x <- identifier
  return (V x)

parseFunction :: Parser Term
parseFunction = do
  f <- identifier
  keyword "("
  t <- sepBy parseTerm (keyword ",")
  keyword ")"
  return (F f t)

parseRule :: Parser Rule
parseRule = do
  l <- parseTerm
  keyword "->"
  r <- parseTerm
  return (l, r)

parseVAR :: Parser ([String], ReplacementMap,TRS)
parseVAR = do
  keyword "VAR"
  xs <- many identifier
  return (xs,[], [])

parseRULES :: Parser ([String], ReplacementMap,TRS)
parseRULES = do
  keyword "RULES"
  rs <- many parseRule
  return ([],[], rs)

parseSTRATEGY' :: Parser (String, [Int])
parseSTRATEGY' = do
  keyword "("
  f <- identifier
  ns <- many nat
  keyword ")"
  return (f, ns)

parseSTRATEGY :: Parser ([String], ReplacementMap, TRS)
parseSTRATEGY = do -- (STRATEGY CONTEXTSENSITIVE ...)
  keyword "STRATEGY"
  keyword "CONTEXTSENSITIVE"
  mu_f <- many parseSTRATEGY'
  return ([], mu_f, [])

parseAnything :: Parser ()
parseAnything =
  -- many1 must not be many.
  -- Otherwise parsec complains at run-time
  -- "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."
  do { _ <- many1 (noneOf "()"); return () } <|>
  do { keyword "("; _ <- many parseAnything; keyword ")" }

parseComment :: Parser ([String], ReplacementMap, TRS)
parseComment = do
  _ <- many parseAnything
  return ([], [], [])

parseSection :: Parser ([String], ReplacementMap,TRS)
parseSection = do
  keyword "("
  (xs, mu, trs) <- try parseVAR <|> try parseRULES <|> try parseSTRATEGY <|> parseComment
  keyword ")"
  return (xs, mu, trs)

parseTRS :: Parser (TRS, ReplacementMap)
parseTRS = do
  ps <- many parseSection
  eof
  let (xss, mu, trss) = unzip3 ps in
    return (convert (concat xss) (concat trss), concat mu)

readTRSFile :: String -> IO (Either ParseError (TRS, ReplacementMap))
readTRSFile path = parseFromFile parseTRS path
