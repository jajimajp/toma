module INFParser (readINFProblem, showCES) where
-- Parser for CoCo INF problems
-- copy and paste from Moca...

import CES
import Data.List
import Data.Monoid ((<>)) -- redundant, but for VSCode simple haskell integration
import INFProblem
import ParserTerm as PT
import Text.ParserCombinators.Parsec

type InputData = (String, [String], CES, [String], PES)

-- Parsers

inf_problem :: Parser INFProblem
inf_problem = do
  secs <- many1 section
  return (arrangeData (mergeSections secs))

mergeSections :: [InputData] -> InputData
mergeSections secs 
  = ( [ ch | (t, _, _, _, _) <- secs, ch <- t ],
      [ vr | (_, vrs, _, _, _) <- secs, vr <- vrs ],
      [ cr | (_, _, crs, _, _) <- secs, cr <- crs ],
      [ vc | (_, _, _, vcs, _) <- secs, vc <- vcs ],
      [  c | (_, _, _, _, cs) <- secs, c <- cs ]
    )

arrangeData :: (String, [String], CES, [String], PES) -> INFProblem
arrangeData (t, vrs, crs, vcs, cs)
  | t == "SEMI-EQUATIONAL"  = SemiEquational (crs', cs')
  | t == "JOIN"             = Join           (crs', cs')
  | otherwise               = Oriented       (crs', cs')
  where 
    crs' = varsToConstantsCES vrs crs
    cs' = varsToConstantsES vcs cs

--- sections

section :: Parser InputData
section = do
  keyword "("
  (t, vrs, crs, vcs, cs) 
    <- try sec_comment
        <|> (try sec_problem
          <|> (try sec_ctype
            <|> (try sec_vars_r
              <|> (try sec_rules
                <|> (try sec_vars_c <|> sec_condition)))))
  keyword ")"
  return (t, vrs, crs, vcs, cs) 

sec_problem :: Parser InputData
sec_problem = do 
  keyword "PROBLEM"
  spaces
  _ <- upper_word
  return ([], [], [], [], [])

sec_ctype :: Parser InputData
sec_ctype = do
  keyword "CONDITIONTYPE"
  spaces
  t <- upper_word
  return (t, [], [], [], [])

sec_vars_r :: Parser InputData
sec_vars_r = do
  keyword "VARR"
  spaces
  vrs <- many symbol
  return ([], vrs, [], [], [])

sec_rules :: Parser InputData
sec_rules = do
  keyword "RULES"
  spaces
  crs <- many (try conditional_rule)
  return ([], [], crs, [], [])

sec_vars_c :: Parser InputData
sec_vars_c = do
  keyword "VARC"
  spaces
  vrc <- many symbol
  return ([], [], [], vrc, [])

sec_condition :: Parser InputData
sec_condition = do
  keyword "CONDITION"
  spaces
  cs <- conditions
  return ([], [], [], [], cs)

sec_comment :: Parser InputData
sec_comment = do
  keyword "COMMENT"
  spaces
  skipMany (noneOf ")")
  return ([], [], [], [], [])


condition :: Parser PE
condition = do
  t1 <- term
  keyword "=="
  t2 <- term
  return (t1, t2)

conditions :: Parser PES
conditions = sepBy1 condition (keyword ",")

conditional_rule :: Parser CEquation
conditional_rule = try rule_with_conditions <|> rule_without_conditions

rule_with_conditions :: Parser CEquation
rule_with_conditions = do
  r <- rule
  keyword "|"
  cs <- conditions
  return (cs, r)
  -- return ([], r)

rule_without_conditions :: Parser CEquation
rule_without_conditions = do
  r <- rule
  return ([], r)

rule :: Parser PE
rule = do
  t1 <- term 
  keyword "->"
  t2 <- term
  return (t1, t2)

term :: Parser Term
term = try function <|> var_or_const

symbol :: Parser String 
symbol = do
  spaces
  x <- many1 (noneOf "(), \n\r\t\\|\"")
  spaces
  return x

var_or_const :: Parser Term
var_or_const = do
  x <- symbol
  return (V x)

function :: Parser Term
function = do
  f <- symbol
  keyword "("
  ts <- sepBy term (keyword ",")
  keyword ")"
  return (F f ts)


alpha_numeric :: Parser Char
alpha_numeric = try alphaNum <|> oneOf "-_"

upper_word :: Parser String
upper_word = do
  spaces
  c <- upper
  s <- many alpha_numeric
  spaces
  return (c : s)


keyword :: String -> Parser ()
keyword s = do
  spaces
  _ <- string s
  spaces
  return () 


varsToConstants :: [String] -> Term -> Term
varsToConstants xs t = PT.substitute t sgm
  where sgm = [ (x, F x []) | x <- PT.variables t, x `notElem` xs ]

varsToConstantsPair :: [String] -> PE -> PE
varsToConstantsPair xs (s, t)  = (varsToConstants xs s, varsToConstants xs t)

varsToConstantsES :: [String] -> PES -> PES
varsToConstantsES xs es = [ varsToConstantsPair xs e | e <- es ]

varsToConstantsCES :: [String] -> CES -> CES
varsToConstantsCES xs ces = [ (varsToConstantsES xs es, varsToConstantsPair xs e)  | (es, e) <- ces ]


-- Reader
readINFProblem :: String -> IO (Either ParseError INFProblem)
readINFProblem path = do
  s <- readFile path
  let result = parse inf_problem "" (renameVAR s)
  case result of
    Left err -> return (Left err)
    Right infp -> return (Right infp)


renameVAR :: String -> String
renameVAR [] = []
renameVAR s | "VAR" `isPrefixOf` s, earlier s == "RULES"   
  = "VARR" ++ renameVAR (drop (length "VAR") s)
renameVAR s | "VAR" `isPrefixOf` s, earlier s == "CONDITION"   
  = "VARC" ++ renameVAR (drop (length "VAR") s)
renameVAR (c : s') = c : renameVAR s'

earlier :: String -> String
earlier [] = []
earlier s | "RULES"     `isPrefixOf` s  = "RULES"
          | "CONDITION" `isPrefixOf` s  = "CONDITION"
earlier (_c : s') = earlier s'
