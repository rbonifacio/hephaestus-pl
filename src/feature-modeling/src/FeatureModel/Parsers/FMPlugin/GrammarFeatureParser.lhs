\begin{code}
module FetureModel.Parsers.GrammarFeatureParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

data Production = Production { 
 idp :: String,
 children  :: [Production]
} deriving (Show)

-- 
-- parses a char, but ignores spaces.
--
charWithoutSpace :: Char -> Parser Char
charWithoutSpace c =  
 do { skipMany space; 
      r <- char c; 
      skipMany space;
      return r
    }	
  	
--
-- A simple parser for identifiers.
-- This parser recongnizes strings with the following 
-- pattern:
--
--  identifier :: char(alphanum)*
--
-- It is important to notice that this parser does not recongnize 
-- strings starting with a number.
--	 
identifier :: Parser String		
identifier = 
 do {c <- letter;
      s <- many alphaNum;
      return (c : s)
    }	

gdslProduction :: Parser Production 
gdslProduction = 
 do {
   skipMany space;
   p <- identifier;
   skipMany space;
   char ':';
   skipMany space;
   ps <- sepBy1 gdslTerm pSeparator;
   char ';';
   return Production { idp = p, children = ps }
 }

gdslTerm :: Parser Production
gdslTerm = 
 do { 
   skipMany space;
   t <- identifier;
   skipMany space;
   return Production { idp = t, children = [] } 
 } <|>
 do {
   skipMany space;
   ts <- sepBy1 gdslToken tSeparator;
   string "::";
   s <- gdslPattern;
   return Production { idp = s, children = [Production { idp = x , children = [] } | x <- ts ] }  
 } 

gdslPattern :: Parser String
gdslPattern = 
 do {
  skipMany space;
  t <- identifier; skipMany space;
  return t;
 }
 
gdslToken :: Parser String 
gdslToken = 
 do {
  char '[';
  t <- identifier;
  char ']';
  return (t ++ "-")
 } <|>
 do {
  t <- identifier;
  do { try (char '+'); return (t ++ "+") } <|> 
  do { try (char '*'); return (t ++ "*") } <|>
  return t
 } 

tSeparator :: Parser ()
tSeparator = skipMany1 space

pSeparator :: Parser () 
pSeparator = skipMany1 (char ',')

run :: Show a => Parser a -> String -> IO ()
run p input = 
 case (parse p " " input ) of 
  Right e -> print e
  Left err -> do { putStr "parse error at "; print err }

\end{code}