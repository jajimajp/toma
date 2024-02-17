module UEQParser 
  (Relation (..),
   Formula,
   AnnotatedFormula, 
   Declaration (..), 
   -- showAnnotatedFormulas,
   readUEQFile) where

-- NOTE: the BNF is provided at https://tptp.org/TPTP/SyntaxBNF.html

import Prelude hiding (negate)
import ParserTerm
import Text.ParserCombinators.Parsec
import System.Directory

data Relation = Eq | Neq

type Formula = (Relation, (Term, Term))

type AnnotatedFormula = (String, String, Formula)

data Declaration = CNF AnnotatedFormula | Include String

-- Manipilation functions

negate :: Formula -> Formula
negate (Eq,  e) = (Neq, e)
negate (Neq, e) = (Eq,  e)

-- Pretty printers

-- showFormula :: Formula -> String
-- showFormula (Eq,  (s, t)) = show s ++ " = "  ++ show t
-- showFormula (Neq, (s, t)) = show s ++ " != " ++ show t

-- showAnnotatedFormula :: AnnotatedFormula -> String
-- showAnnotatedFormula (clauseName, role, f) =
--   "cnf(" ++ clauseName ++ ", " ++ role ++ ", " ++ showFormula f ++ ")."

-- showAnnotatedFormulas :: [AnnotatedFormula] -> String
-- showAnnotatedFormulas afs =
--   unlines [ showAnnotatedFormula af | af <- afs ]


-- Tokenizers

name :: Parser String
name = try atomic_word <|> integer

atomic_word :: Parser String
atomic_word = try lower_word <|> single_quoted

integer :: Parser String
integer = many1 digit

alpha_numeric :: Parser Char
alpha_numeric = try alphaNum <|> char '_'

lower_word :: Parser String
lower_word = do
  _ <- spaces_or_comments
  c <- lower
  s <- many alpha_numeric
  _ <- spaces_or_comments
  return (c : s)

upper_word :: Parser String
upper_word = do
  spaces_or_comments
  c <- upper
  s <- many alpha_numeric
  spaces_or_comments
  return (c : s)

single_quoted :: Parser String
single_quoted = do
  spaces_or_comments
  _ <- char '\''
  s <- many char_in_single_quote
  _ <- char '\''
  spaces_or_comments
  return s

char_in_single_quote :: Parser Char
char_in_single_quote = try escaped_char_in_single_quote <|> noneOf "\\'"

escaped_char_in_single_quote :: Parser Char
escaped_char_in_single_quote = do
  _ <- char '\\'
  c <- oneOf "\\'"
  return c

comment :: Parser ()
comment = do
  _ <- char '%'
  _ <- many (noneOf "\n")
  _ <- char '\n'
  return ()

spaces_or_comments :: Parser ()
spaces_or_comments = do
  _ <- many (try (do { _ <- space; return () }) <|> comment)
  return ()

keyword :: String -> Parser ()
keyword s = do
  spaces_or_comments
  _ <- string s
  spaces_or_comments
  return () 

paren :: Parser a -> Parser a
paren p = do
  keyword "("
  x <- p
  keyword ")"
  return x

-- Parsers for terms and formulas.

parseTerm :: Parser Term
parseTerm = try parseVariable <|> try parseFunctionApplication <|> parseConstant 

parseVariable :: Parser Term
parseVariable = do
  s <- upper_word
  return (V s)

constant :: Parser String
constant = functor

functor :: Parser String
functor = atomic_word

parseConstant :: Parser Term
parseConstant = do
  s <- constant
  return (F s [])

parseFunctionApplication :: Parser Term
parseFunctionApplication = do
  f <- functor
  ts <- paren (sepBy parseTerm (keyword ","))
  return (F f ts)

parseEq :: Parser Relation
parseEq = do
  keyword "="
  return Eq

parseNeq :: Parser Relation
parseNeq = do
  keyword "!="
  return Neq

parseRelation :: Parser Relation
parseRelation = try parseEq <|> parseNeq

parseAtom :: Parser Formula
parseAtom = do
  s <- parseTerm
  r <- parseRelation
  t <- parseTerm
  return (r, (s, t))

parseNegativeLiteral :: Parser Formula
parseNegativeLiteral = do
  keyword "~"
  f <- parseAtom 
  return (negate f)

parseLiteral :: Parser Formula
parseLiteral = try parseNegativeLiteral <|> parseAtom

parseFormula :: Parser Formula
parseFormula = try (paren parseFormula) <|> parseLiteral

-- Parsers for declarations.

parseCNF :: Parser Declaration
parseCNF = do
  keyword "cnf"; keyword "("
  clauseName <- name
  keyword ","
  role <- lower_word
  keyword ","
  formula <- parseFormula
  keyword ")"
  keyword "."
  return (CNF (clauseName, role, formula))

parseInclude :: Parser Declaration
parseInclude = do
  keyword "include"
  keyword "("
  filename <- single_quoted
  keyword ")"
  keyword "."
  return (Include filename)

parseDeclaration :: Parser Declaration
parseDeclaration = try parseCNF <|> parseInclude

parseToplevel :: Parser [Declaration]
parseToplevel = do
  ds <- many parseDeclaration
  eof
  return ds

-- Reading files.

lookupFile :: String -> [String] -> IO (Maybe String)
lookupFile filename [] = do
  b <- doesFileExist filename -- interprete as absolute path
  if b
    then return (Just filename)
    else return Nothing
lookupFile filename (dir : dirs) = do
  let path = dir ++ "/" ++ filename
  b <- doesFileExist path
  if b 
    then return (Just path)
    else lookupFile filename dirs


readUEQFile :: [String] -> String -> IO (Either ParseError [AnnotatedFormula])
readUEQFile dirs filename = do
  m <- lookupFile filename dirs
  case m of
    Nothing -> error ("File not found: " ++ filename)
    Just path -> do
      result <- parseFromFile parseToplevel path
      case result of
        Left e -> return (Left e)
        Right declarations -> preprocess dirs declarations

preprocess :: [String] -> [Declaration] -> IO (Either ParseError [AnnotatedFormula])
preprocess _ [] = return (Right [])
preprocess dirs (CNF af : ds) = do
  result <- preprocess dirs ds
  case result of
    Left e    -> return (Left e)
    Right afs -> return (Right (af : afs))
preprocess dirs (Include path : ds) = do
  result1 <- readUEQFile dirs path
  case result1 of
    Left e1 -> return (Left e1)
    Right afs1 -> do
      result2 <- preprocess dirs ds
      case result2 of
        Left e2 -> return (Left e2)
        Right afs2 -> return (Right (afs1 ++ afs2))
