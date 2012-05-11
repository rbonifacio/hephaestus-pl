%
% This is the Literate Haskell that defines the main module of the 
% FeatureModeling library.
% 
% author: Rodrigo Bonifacio - rbonifacio@computer.org
%
% 2008/2009
%


\documentclass[12pt]{article}
\usepackage{a4wide}

%include polycode.fmt
%include lhs2tex.sty

%format |=> = "\Rightarrow"


\title{A Haskell Library for Feature Modeling}

\author{Rodrigo Bonifacio and Paulo Borba \\ {rba2,phmb}@@cin.ufpe.br}

\begin{document}

\maketitle

\begin{abstract}
We present a Haskell library for feature modeling. Among other capabilities, 
using this library, we are able to: (a) import a feature model generated using the 
Feature Modeling plugin, (b) check if a feature model is satisfiable, and (c) 
check if a product configuration is a valid instance of a feature model. Additionally, we 
argue that extending this library to support some types of \emph{reasoning about feature models}  
would be straightforward. We assume that a reader of this report has a basic knowledge on 
feature models.   
\end{abstract}

\section{Introduction}
In order to test this library, we provide a command line application. The following options 
might be used for running it:

\begin{description}
\item[Feature Model test of satisfiability:] enter --fmIsSatisfiable <path-to-feature-model>
\item[Feature Model type checker:] enter --fmTypeChecker <path-to-feature-model>
\item[Feature Configuration type checker:] enter --fcTypeChecker <path-to-feature-model> <path-to-feature-configuration>
\end{description}

\begin{code}
module Main where

import FeatureModel.Types
import FeatureModel.FMTypeChecker
import FeatureModel.FCTypeChecker

import FeatureModel.Parsers.GenericParser

import Funsat.Types

import Data.Set

import Maybe

import Test.BenchPress

import IO 
import System
import System.Console.GetOpt

-- execIsSatisfiable :: [String] -> IO ()
-- execIsSatisfiable []  = print "expecting a file name. try --help"
 
-- execIsSatisfiable otherwhise = print "command line error. try --help"

-- present a summary of the feature model.
execSummary :: FeatureModel -> IO ()
execSummary fmodel = do print $ summary fmodel

-- check if the feature model is well typed.
execCheck1 :: FeatureModel -> IO () 
execCheck1 fmodel = do print $ fmTypeChecker fmodel

-- translate the fm to cnf and run the
-- minisat solver.
execMinisat :: String -> FeatureModel -> IO() 
execMinisat out fmodel = do 
 execFm2Cnf out fmodel 
 code <- System.system ("minisat " ++ out)
 print $ code

execSAT :: FeatureModel -> IO ()
execSAT fmodel = do print $ fmSATSolver fmodel

-- translate the fm to a dimacs cnf 
-- file.
execFm2Cnf :: String -> FeatureModel -> IO ()
execFm2Cnf out fmodel = do
  let cnf = dimacsFormat (fmToTseitinEncode fmodel)
  let nv = numVars cnf
  let nc = numClauses cnf
  let c  = toList (clauses cnf)
  h <- (openFile out WriteMode)  
  printCnfHeader h nv nc
  printCnfClauses h c
  hClose h
 where 
  printCnfHeader  h nv nc =  do hPutStrLn h (concat ["p cnf", " " ++ (show nv),  " " ++ (show nc)])
  printCnfClauses h [] = return ()
  printCnfClauses h (x:xs) = do
   hPutStrLn h ((concat [(show c) ++ " " | c <- x]) ++  " 0")
   printCnfClauses h xs

main :: IO ()
main = do
 args <- getArgs
 case getOpt Permute options args of
  ( [], [], [] ) -> error $ usageInfo header options
  ( flags, [] , [] ) -> processFlags flags
  ( _, nonOpts, [] ) -> error $ concat ["\nunrecognized options ", unwords nonOpts, (usageInfo header options)]
  ( _, _, msgs) -> error $ concat msgs ++ usageInfo header options

processFlags :: [Flag] -> IO ()
processFlags flags = do
 let args    = getOptions flags defaultOptions
 fmodel <- parseFeatureModel args
 case (cmd args) of
  "summary" -> execSummary (fmodel)
  "check1"  -> execCheck1  (fmodel)
  "check2"  -> execCheck1  (fmodel)
  "fm2cnf"  -> execFm2Cnf  (fc args) (fmodel)
  "minisat" -> execMinisat (fc args) (fmodel)
  "sat"     -> execSAT (fmodel)
  "find-bad-smells" -> print $ findBadSmells fmodel
  otherwise -> error $ concat ["\nunrecognized command ", (cmd args), (usageInfo header options)]

parseFeatureModelFormat args = 
  case (fmt args) of 
   "fmp"       -> FMPlugin
   "fmide"     -> FMIde
   "fmgrammar" -> FMGrammar
   "sxfm"      -> SXFM
   otherwise -> error $ concat ["\nunrecognized format ", (fmt args), (usageInfo header options)]
 
parseFeatureModel args = do
 let fileName = fm args
 let format = parseFeatureModelFormat args
 fm <- genericParser fileName format
 return fm



data Flag = Format String 
          | Command String 
          | FMFile String 
          | FCFile String
 deriving (Show)

options :: [OptDescr Flag]
options = [ 
  Option ['f'] ["format"] 
         (OptArg optformat "FMT") 
         ("Set the format option. Use (fmide) for FMIde models" ++
          "\nor (fmp) for FMPlugin models. The defaul option" ++
         "\nis fmp.\n\n") , 
  Option ['c'] ["command"] 
         (ReqArg Command "CMD")
         ("Identify the command to be performed. Use \"summary\" " ++
          "\nfor presenting a summary of the specified feature model" ++
          "\nUse \"fm2cnf\" to export the feature model to dimacs CNF." ++
          "\nUse \"check1\" for checking the specified feature model.  " ++
          "\nFinally, use \"check2\" for checking the specified " ++  
          "\nfeature model and feature configuration.\n\n"),
  Option [] ["file1"] 
         (ReqArg FMFile "FILE")
         "Identify the feature model file.\n\n",
  Option [] ["file2"] 
         (ReqArg FCFile "FILE")
         ("Depending on the command option, identify either the feature" ++
          "\nconfiguration file (check2 command) or the output file" ++
          "\n(fm2cnf command).\n\n")
 ]

optformat :: Maybe String -> Flag
optformat = Format . fromMaybe "fmp" 

data Options = Options {
  fmt :: String,
  cmd :: String,
  fm  :: String,
  fc  :: String
} deriving (Show)

defaultOptions = Options {
  fmt = "fmp",
  cmd = "",
  fm  = "",
  fc = ""
}

getOptions [] options = options
getOptions (x:xs) options = 
 case x of  
  (Format s)  -> getOptions xs (options { fmt = s })
  (Command s) -> getOptions xs (options { cmd = s })
  (FMFile s)  -> getOptions xs (options { fm  = s })
  (FCFile s)  -> getOptions xs (options { fc  = s })

header = "\nUsage: hfm [OPTION...]\n"
\end{code}

%include Types.lhs

%include FMTypeChecker.lhs

%include FCTypeChecker.lhs

\end{document}
 
