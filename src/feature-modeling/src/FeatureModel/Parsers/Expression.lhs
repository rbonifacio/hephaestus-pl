\begin{code}

module FeatureModel.Parsers.Expression
where 

import FeatureModel.Types 

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

simple :: Parser Char
simple = letter

parseExpression :: Parser FeatureExpression
parseExpression = 
 do { try (string "True") ; skipMany space; 
      return expTrue ;  
    } 
 <|>
 do { try (string "False") ; skipMany space; 
      return expFalse ;  
    }
 <|> 
 do { try (string "Not") ; skipMany space ; 
      char '(' ; skipMany space ; 
      e <- parseExpression ; skipMany space; 
      char ')' ; 
      return (Not e)  
    } 
 <|>
 do { try (string "And") ; skipMany space ; 
      char '(' ; skipMany space ; 
      e1 <- parseExpression ; skipMany space; 
      char ',' ; skipMany space; 
      e2 <- parseExpression ; skipMany space; 
      char ')' ; 
      return (And e1 e2) 
 }
 <|>
 do { try (string "Or") ; skipMany space ; 
      char '(' ; skipMany space ; 
      e1 <- parseExpression ; skipMany space; 
      char ',' ; skipMany space; 
      e2 <- parseExpression ; skipMany space; 
      char ')' ; 
      return (Or e1 e2) 
 }
 <|>
 do { idr <- parseName ; skipMany space; 
      return (FeatureRef idr)  
    } 
 
parseId :: Parser String 
parseId = do { char '(' ; skipMany space ;
               i <- parseName ; skipMany space ;
               char ')' ; skipMany space ;
               return i 
             }

parseName :: Parser String
parseName = do { c <- letter ; 
                 s <- many (alphaNum <|> char '_' <|> char ' ');
                 return (c : s)
               }        

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x


\end{code}