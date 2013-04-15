{-# LANGUAGE RankNTypes #-}
module BasicTypes
where 

import qualified System.FilePath.Posix as P
import qualified System.FilePath.Windows as W
 
import Control.Monad (MonadPlus(..), liftM)

import Network.URI

import Data.List

import System.FilePath
import Data.Maybe 

type Id = String
type Name = String
type Path = String


-- | A generic data type for dealing with parsers.
--   Actually, this is an idiom, being present in several Haskell parsers.
--   Since parsers usually have to perform some kind of IO, the ParserResult 
--   data type shuld be an instance of Monad.
-- 
--   As a final remark, the result of a parser might be either a Sucess or a Fail.
    
data ParserResult a = Success a | Fail String
 deriving (Read, Show, Eq, Ord)

instance Monad ParserResult where 
 return = Success
 fail = Fail
 Success a >>= f = f a
 Fail s    >>= f = Fail s

instance Functor ParserResult where 
 fmap = liftM

instance MonadPlus ParserResult where
  mzero = Fail "Err.mzero"
  mplus (Fail _) y = y
  mplus x _ = x

isSuccess :: ParserResult a -> Bool
isSuccess (Success _) = True
isSuccess (Fail _) = False

showError :: ParserResult a -> String
showError (Success _) = ""
showError (Fail s) = s 
-- 

split :: Char -> String -> [String]
split c "" = [""] 
split c s = 
 if any (== c) s then 
  (delete c (takeWhile (/= c) s)) : split c (tail (dropWhile (/= c) s))
 else [s]
 
splitAndRemoveBlanks :: Char -> String -> [String] 
splitAndRemoveBlanks c s = [filter (/= ' ') x | x <- (split c s)] 

splitAndTrim :: Char -> String -> [String]
splitAndTrim ch str = [trim x | x <- (split ch str)]
 
trim :: String -> String
trim str 
 | (last str) == ' ' = trim (removeLast str)
 | (head str) == ' ' = trim (tail str)
 | otherwise = str
 
removeLast :: String -> String
removeLast (h:t) 
 | (length t) == 1 = []
 | otherwise = h:(removeLast t)

existsWord:: String -> String -> Bool 
existsWord w s  = 
 let ws = parseString s
 in  w `elem` ws

replaceString :: String -> String -> String -> String
replaceString old new str = 
 let ws = parseString str
 in unparseString (replaceString' old new ws)
 where 
  replaceString' o n [] = []
  replaceString' o n (w:ws) = (if (w == o) then n else w) : replaceString' o n ws


concatBefore :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
concatBefore _ _ [] = []
concatBefore f ls (x:xs) = 
 if f x 
  then (ls ++ [x]) ++ (concatBefore f ls xs) 
  else x : (concatBefore f ls xs)

concatAfter :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
concatAfter _ _ [] = []
concatAfter f ls (x:xs) = 
 if f x
  then x : (ls ++ xs)
  else x : (concatAfter f ls xs)

concatAround :: Eq a => (a -> Bool) -> (a -> Bool) -> [a] -> [a] -> [a]
concatAround _ _ _ [] = []
concatAround match cp ls (x:xs) = 
 if match x 
  then concat [proceed cp l (x:xs) | l <- ls]
  else x : concatAround match cp ls xs
 where 
  proceed c l x = if (c l) then x else [l]

parseString :: String -> [String]
parseString s  = parseString' (s, "", [])

parseString' :: (String, String, [String]) -> [String]
parseString' ("", s2, ss) = (ss ++ [s2])
parseString' ((x:xs), s2, ss) = 
   if x `elem` [',', '.', ' ', '-'] 
    then parseString' (xs, "", (ss ++ [s2]) ++ [(x:"")])
    else parseString' (xs, s2 ++ (x:""), ss)

unparseString :: [String] -> String 
unparseString xs = concat xs


createURI :: String -> String
createURI f@(x:':':ys) = createFromWindowsPath f
createURI f = createFromPosixPath f 
  
createFromWindowsPath f = 
 let 
  driver = W.takeDrive f
  directories = replaceString " " "/" (unwords (tail (W.splitDirectories f)))
 in  
  "file:///" ++ ( (head driver) : ":/" ) ++ directories 

createFromPosixPath f = "file://" ++ f

getFileExtension :: String -> String 
getFileExtension = P.takeExtension  

-- this function is useful for defining 
-- precedences in a list that could not be ordered 
-- by just defining its basic type as an instance of the 
-- Order class.
--
-- examples:
-- 
--  precedence [(a2, a1)] [a5, a3, a1, a2] = [a5, a3, a2, a1]
--  precedence [(a2, a1), (a1,a5)]) [a5, a3, a2, a1] = 

precedence :: Eq a => [(a,a)] -> [a] -> [(a,Integer)]
precedence _ [] = []
precedence ps ls = 
 let 
   ls' = [(l,0) | l <- ls]
 in precedence' ps ls'

precedence' []  ls = [l | l<- ls]
precedence' (p:ps) ls = 
 let 
  px = [x | x <- ls, fst p == fst x]
  py = [y | y <- ls, snd p == fst y]
 in 
   case (px,py) of
    ([x],[y]) -> precedence' ps [if (fst y == fst l) then (fst y, (+1) (snd x)) else l | l <- ls]
    otherwise -> ls

type OrderedTuple = Eq a => (a,Integer)

parenthesize :: [String] -> String
parenthesize [] = "-" 
parenthesize ss = "(" ++ parenthesize' ss ++ ")" 
 where 
  parenthesize'  [] = ""
  parenthesize' [l] = l 
  parenthesize' (l:ls) = l ++ ", " ++ parenthesize' ls


-----------------------------------------------------------------------------------------
-- definitions brought from module Main.hs of Hephaestus 
-----------------------------------------------------------------------------------------

type PropertyValue = (String, String)

readPropertyValue :: String -> Maybe PropertyValue
readPropertyValue s =
 let p = break (== '=') s
 in case p of 
     ([], _) -> Nothing
     (k , v) -> Just (k, tail v)  

findPropertyValue:: String -> [PropertyValue] -> Maybe PropertyValue  
findPropertyValue k = listToMaybe . filter (\x -> k == fst x) 




