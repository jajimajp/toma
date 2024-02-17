module SMTParser (parseSMTOutput) where

import Text.ParserCombinators.Parsec
import Control.Monad

import SMT

parseSMTOutput :: Parser SMTOutput
parseSMTOutput = try parseSat <|> parseUnsat

parseSat :: Parser SMTOutput
parseSat = do
  spaces
  void $ string "sat"
  spaces
  void $ char '('
  spaces
  bindings <- many parseBinding
  spaces
  void $ char ')'
  spaces
  return $ Just bindings

parsePosInt :: Parser SMTValue
parsePosInt = do
  spaces
  i <- many1 digit
  spaces
  return (SMTIntValue (read i))

parseNegInt :: Parser SMTValue
parseNegInt = do
  spaces
  void $ char '('
  spaces
  void $ char '-'
  spaces
  i <- many1 digit
  spaces
  void $ char ')'
  spaces
  return (SMTIntValue (- (read i)))

parseInt :: Parser SMTValue
parseInt = try parsePosInt <|> parseNegInt

parseTrue :: Parser SMTValue
parseTrue = do
  void $ string "true"
  return $ SMTBoolValue True

parseFalse :: Parser SMTValue
parseFalse = do
  void $ string "false"
  return $ SMTBoolValue False

parseBool :: Parser SMTValue
parseBool = parseTrue <|> parseFalse

parseBinding :: Parser (String, SMTValue)
parseBinding = do
  spaces
  void $ char '('
  spaces
  varname <- manyTill (noneOf " \t\r\n") space
  spaces
  val <- parseInt <|> parseBool
  spaces
  void $ char ')'
  spaces
  return (varname, val)

parseUnsat :: Parser SMTOutput
parseUnsat = do
  spaces
  void $ string "unsat"
  spaces
  return Nothing
