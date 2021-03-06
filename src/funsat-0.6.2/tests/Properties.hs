{-# OPTIONS_GHC -fglasgow-exts #-}
module Properties where

{-
    This file is part of funsat.

    funsat is free software: it is released under the BSD3 open source license.
    You can find details of this license in the file LICENSE at the root of the
    source tree.

    Copyright 2008 Denis Bueno
-}

import Funsat.Solver hiding ((==>))

import Control.Exception( assert )
import Control.Monad
import Data.Array.Unboxed
import Data.Bits hiding( xor )
import Data.Foldable hiding (sequence_)
import Data.List (nub, splitAt)
import Data.Maybe
import Data.Set( Set )
import Debug.Trace
import Funsat.Circuit hiding( Circuit(..) )
import Funsat.Circuit( Circuit(input,true,false,ite,xor,onlyif) )
import Funsat.Types
import Funsat.Utils
import Language.CNF.Parse.ParseDIMACS( parseFile )
import Prelude hiding ( or, and, all, any, elem, minimum, foldr, splitAt, concatMap, sum, concat )
import Funsat.Resolution( ResolutionTrace(..) )
import System.IO
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen( unGen )

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Funsat.Resolution as Resolution
import qualified Language.CNF.Parse.ParseDIMACS as ParseCNF
import qualified Test.QuickCheck as QC
import qualified Funsat.Circuit as C
import qualified Funsat.Circuit as Circuit


main :: IO ()
main = do
      --setStdGen (mkStdGen 42)
      hPutStr stderr "prop_randAssign: " >> check config prop_randAssign
      hPutStr stderr "prop_allIsTrueUnderA: " >> check config prop_allIsTrueUnderA
      hPutStr stderr "prop_noneIsFalseUnderA: " >> check config prop_noneIsFalseUnderA
      hPutStr stderr "prop_noneIsUndefUnderA: " >> check config prop_noneIsUndefUnderA
      hPutStr stderr "prop_negIsFalseUnder: " >> check config prop_negIsFalseUnder
      hPutStr stderr "prop_negNotUndefUnder: " >> check config prop_negNotUndefUnder
      hPutStr stderr "prop_outsideUndefUnder: " >> check config prop_outsideUndefUnder
      hPutStr stderr "prop_clauseStatusUnderA: " >> check config prop_clauseStatusUnderA
      hPutStr stderr "prop_negDefNotUndefUnder: " >> check config prop_negDefNotUndefUnder
      hPutStr stderr "prop_undefUnderImpliesNegUndef: " >> check config prop_undefUnderImpliesNegUndef
      hPutStr stderr "prop_count: " >> check config prop_count
      hPutStr stderr "prop_circuitToCnf: " >> check config{ maxSize = 10000} prop_circuitToCnf
      hPutStr stderr "prop_circuitSimplify: " >> check config prop_circuitSimplify

      -- Add more tests above here.  Setting the rng keeps the SAT instances the
      -- same even if more tests are added above.  I want this because if I make
      -- a change that makes the solver dramatically faster or slower, I know
      -- this wasn't due to the test distribution.
      gen <- getStdGen
      setStdGen (mkStdGen 42)
      hPutStr stderr "prop_solveCorrect: "
      check solveConfig prop_solveCorrect

      setStdGen gen
      hPutStr stderr "prop_solveCorrect (rand): "
      check solveConfig prop_solveCorrect
      gen <- getStdGen

      setStdGen (mkStdGen 42)
      hPutStr stderr "prop_resolutionChecker: "
      check resChkConfig prop_resolutionChecker

      setStdGen gen
      hPutStr stderr "prop_resolutionChecker (rand): "
      check resChkConfig prop_resolutionChecker

check :: Testable prop => Args -> prop -> IO ()
check = QC.quickCheckWith
config = QC.stdArgs{ maxSuccess = 1400
                   , maxSize    = 800
                   , maxDiscard = 1000 }

-- Special configuration for the "solve this random instance" tests.
solveConfig  = config{ maxSuccess = 2000 }
resChkConfig = config{ maxSuccess = 1200, maxSize = 600 }

myConfigEvery testnum args = show testnum ++ ": " ++ show args ++ "\n\n"

trivial :: Testable p => Bool -> p -> Property
trivial t = classify t "trivial"

-- * Tests
prop_solveCorrect (cnf :: CNF) =
    trivial (numClauses cnf < 2 || numVars cnf < 2) $
    classify (numClauses cnf > 15 || numVars cnf > 10) "c>15, v>10" $
    classify (numClauses cnf > 30 || numVars cnf > 20) "c>30, v>20" $
    classify (numVars cnf > 20) "c>30, v>30" $
    case solve defaultConfig cnf of
      (Sat m,_,rt) -> label "SAT" $ verifyBool (Sat m) rt cnf
      (Unsat _,_,rt) -> label "UNSAT" $
                        case Resolution.checkDepthFirst (fromJust rt) of
                          Left e ->
                                trace ("rt = " ++ show rt ++ "\n"
                                       ++ "Resolution checker error: " ++ show e)
                              $ False
                          Right _ -> True

prop_resolutionChecker (cnf :: UnsatCNF) =
    case solve1 (unUnsatCNF cnf) of
      (Sat _,_,_)    -> label "SAT (unverified)" True
      (Unsat _,_,rt) -> label "UNSAT" $
          case Resolution.genUnsatCore (fromJust rt) of
            Left _e -> False
            Right unsatCore ->
                case solve1 ((unUnsatCNF cnf){ clauses = Set.fromList unsatCore}) of
                  (Sat _,_,_) -> False
                  (Unsat _,_,_) -> True

prop_allIsTrueUnderA (m :: IAssignment) =
    allA (\i -> if i /= 0 then L i `isTrueUnder` m else True) m

prop_noneIsFalseUnderA (m :: IAssignment) =
    not $ anyA (\i -> if i /= 0 then L i `isFalseUnder` m else False) m

prop_noneIsUndefUnderA (m :: IAssignment) =
    not $ anyA (\i -> if i /= 0 then L i `isUndefUnder` m else False) m

prop_negIsFalseUnder (m :: IAssignment) =
    allA (\l -> if l /= 0 then negate (L l) `isFalseUnder` m else True) m

prop_negNotUndefUnder (m :: IAssignment) =
    allA (\l -> if l /= 0 then not (negate (L l) `isUndefUnder` m) else True) m

prop_outsideUndefUnder (l :: Lit) (m :: IAssignment) =
    trivial ((unVar . var) l > rangeSize (bounds m)) $
    inRange (bounds m) (var l) ==>
    trivial (m `contains` l || m `contains` negate l) $
    not (m `contains` l) && not (m `contains` (negate l)) ==>
    l `isUndefUnder` m

prop_negDefNotUndefUnder (l :: Lit) (m :: IAssignment) =
    inRange (bounds m) (var l) ==>
    m `contains` l || m `contains` (negate l) ==>
    l `isTrueUnder` m || negate l `isTrueUnder` m

prop_undefUnderImpliesNegUndef (l :: Lit) (m :: IAssignment) =
    inRange (bounds m) (var l) ==>
    trivial (m `contains` l) $
    l `isUndefUnder` m ==> negate l `isUndefUnder` m
    

prop_clauseStatusUnderA (c :: Clause) (m :: IAssignment) =
    classify expectTrueTest "expectTrue"$
    classify expectFalseTest "expectFalseTest"$
    classify expectUndefTest "expectUndefTest"$
    if expectTrueTest then c `isTrueUnder` m
    else if expectFalseTest then c `isFalseUnder` m
    else c `isUndefUnder` m
        where
          expectTrueTest = not . null $ c `List.intersect` (map L $ elems m)
          expectFalseTest = all (`isFalseUnder` m) c
          expectUndefTest = not expectTrueTest && not expectFalseTest

-- Verify assignments generated are sane, i.e. no assignment contains an
-- element and its negation.
prop_randAssign (a :: IAssignment) =
    not $ anyA (\l -> if l /= 0 then a `contains` (negate $ L l) else False) a

-- unitPropFar should stop only if it can't propagate anymore.
-- prop_unitPropFarthest (m :: Assignment) (cnf :: CNF) =
--     label "prop_unitPropFarthest"$
--     case unitPropFar m cnf of
--       Nothing -> label "no propagation" True
--       Just m' -> label "propagated" $ not (anyUnit m' cnf)

-- Unit propagation may only add to the given assignment.
-- prop_unitPropOnlyAdds (m :: Assignment) (cnf :: CNF) =
--     label "prop_unitPropOnlyAdds"$
--     case unitPropFar m cnf of
--       Nothing -> label "no propagation" True
--       Just m' -> label "propagated" $ all (\l -> elem l m') m


(<==>) = iff
infixl 3 <==>


prop_count p xs =
    count p xs == length (filter p xs)
        where _types = xs :: [Int]

prop_argmin f x y =
    f x /= f y ==>
      argmin f x y == m
  where m = if f x < f y then x else y

instance Show (a -> b) where
    show = const "<fcn>"


-- ** Circuits and CNF conversion

-- If CNF generated from circuit satisfiable, check that circuit is by that
-- assignment.
prop_circuitToCnf :: Circuit.Tree Var -> Property
prop_circuitToCnf treeCircuit =
    let pblm@(CircuitProblem{ problemCnf = cnf }) =
            toCNF . runShared . castCircuit $ treeCircuit
        (solution, _, _) = solve1 cnf
    in case solution of
         Sat{} -> let benv = projectCircuitSolution solution pblm
                  in label "Sat"
                     . trivial (Map.null benv)
                     $ runEval benv (castCircuit treeCircuit)

         Unsat{} -> label "Unsat (unverified)" True

-- circuit and simplified version should evaluate the same
prop_circuitSimplify :: ArbBEnv -> Circuit.Tree Var -> Property
prop_circuitSimplify (ArbBEnv benv) c =
    trivial (c == TTrue || c == TFalse) $
    assert (treeVars c `Set.isSubsetOf` Map.keysSet benv) $
      runEval benv (castCircuit c)
      == runEval benv (castCircuit . simplifyTree $ c)

{-
prop_circuitGraphIsTree :: C.Shared Var -> Property
prop_circuitGraphIsTree sh = c `equivalentTo` g
  where
  equivalentTo = undefined
  g = castCircuit c :: Graph Var
  c = C.runShared sh
-}

treeVars :: (Ord v) => Circuit.Tree v -> Set v
treeVars = C.foldTree (flip Set.insert) Set.empty




------------------------------------------------------------------------------
-- * Helpers
------------------------------------------------------------------------------



allA :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> Bool
allA p a = all (p . (a !)) (range . bounds $ a)

anyA :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> Bool
anyA p a = any (p . (a !)) (range . bounds $ a)

_findA :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> Maybe e
_findA p a = (a !) `fmap` find (p . (a !)) (range . bounds $ a)


-- Generate exactly n distinct, random things from given enum, starting at
-- element given.  Obviously only really works for infinite enumerations.
uniqElts :: (Enum a) => Int -> a -> Gen [a]
uniqElts n x =
    do is <- return [x..]
       choices <-
           sequence $ map
                      (\i -> do {b <- oneof [return True, return False];
                                 return $ if b then Just i else Nothing})
                      is
       return $ take n $ catMaybes choices

-- Send this as a patch for quickcheck, maybe.
iff :: Bool -> Bool -> Property
first `iff` second =
    classify first "first" $
    classify (not first) "not first" $
    classify second "second" $
    classify (not second) "not second" $
    if first then second
    else not second
    && if second then first
       else not first


fromRight (Right x) = x
fromRight (Left _) = error "fromRight: Left"


_intAssignment :: Int -> Integer -> [Lit]
_intAssignment n i = map nthBitLit [0..n-1]
    -- nth bit of i as a literal
    where nthBitLit n = toLit (n + 1) $ i `testBit` n
          toLit n True  = L n
          toLit n False = negate $ L n
                         


_powerset       :: [a] -> [[a]]
_powerset []     = [[]]
_powerset (x:xs) = xss /\/ map (x:) xss
    where
      xss = _powerset xs

      (/\/)        :: [a] -> [a] -> [a]
      []     /\/ ys = ys
      (x:xs) /\/ ys = x : (ys /\/ xs)


------------------------------------------------------------------------------
-- * Generators
------------------------------------------------------------------------------

instance Arbitrary Var where
    arbitrary = sized $ \n -> V `fmap` choose (1, n)
instance Arbitrary Lit where
    arbitrary = sized $ sizedLit

-- Generates assignment that never has a subset {l, -l}.
instance Arbitrary IAssignment where
    arbitrary = sized assign'
        where 
          assign' n = do lits :: [Lit] <- vector n
                         return $ array (V 1, V n) $ map (\i -> (var i, unLit i))
                                                     (nub lits)

instance Arbitrary CNF where
    arbitrary = sized (genRandom3SAT 3.0)

newtype ArbBEnv = ArbBEnv (BEnv Var) deriving (Show)
instance Arbitrary ArbBEnv where
    arbitrary = sized $ \n -> do
                  bools <- vector (n+1) :: Gen [Bool]
                  return . ArbBEnv $ Map.fromList (zip [V 1 .. V (n+1)] bools)

instance Arbitrary (Tree Var) where
    arbitrary = sized sizedCircuit

sizedLit n = do
  v <- choose (1, n)
  t <- oneof [return id, return negate]
  return $ L (t v)


-- | Generator for a circuit containing at most `n' nodes, involving only the
-- literals 1 .. n.
sizedCircuit :: (Circuit c) => Int -> Gen (c Var)
sizedCircuit 0 = return . input . V $ 1
sizedCircuit n =
    oneof [ return true
          , return false
          , (return . input . V) n
          , liftM2 C.and subcircuit2 subcircuit2
          , liftM2 C.or  subcircuit2 subcircuit2
          , liftM C.not subcircuit1
          , liftM3 ite subcircuit3 subcircuit3 subcircuit3
          , liftM2 onlyif subcircuit2 subcircuit2
          , liftM2 C.iff subcircuit2 subcircuit2
          , liftM2 xor subcircuit2 subcircuit2
          ]
  where subcircuit3 = sizedCircuit (n `div` 3)
        subcircuit2 = sizedCircuit (n `div` 2)
        subcircuit1 = sizedCircuit (n - 1)

-- | Generate a random 3SAT problem with the given ratio of clauses/variable.
--
-- Current research suggests:
--
--  * ~ 4.3: hardest instances
--  * < 4.3: SAT & easy
--  * > 4.3: UNSAT & easy
genRandom3SAT :: Double -> Int -> Gen CNF
genRandom3SAT clausesPerVar n =
    do let nClauses = ceiling (fromIntegral nVars * clausesPerVar)
       clauseList <- replicateM nClauses arbClause
       return $ CNF { numVars    = nVars
                    , numClauses = nClauses
                    , clauses    = Set.fromList clauseList }
  where 
    nVars = n `div` 3
    arbClause :: Gen Clause
    arbClause = do
      a <- sizedLit nVars
      b <- sizedLit nVars
      c <- sizedLit nVars
      return [a,b,c]


windows :: Int -> [a] -> [[a]]
windows n xs = if length xs < n
               then []
               else take n xs : windows n (drop n xs)

permute :: [a] -> Gen [a]
permute [] = return []
permute xs = choose (0, length xs - 1) >>= \idx ->
             case splitAt idx xs of
               (pfx, x:xs') -> do perm <- permute $ pfx ++ xs'
                                  return $ x : perm
               _            -> error "permute: bug"


newtype UnsatCNF = UnsatCNF { unUnsatCNF :: CNF } deriving (Show)
instance Arbitrary UnsatCNF where
    arbitrary = liftM UnsatCNF $ sized (genRandom3SAT 5.19)



getCNF :: Int -> IO CNF
getCNF maxVars = do g <- newStdGen
                    return (unGen arbitrary g (maxVars * 3))

prob :: IO ParseCNF.CNF
prob = do cnfOrError <- parseFile "./tests/problems/uf20/uf20-0119.cnf"
          case cnfOrError of
            Left err -> error . show $ err
            Right c  -> return c


-- | Convert parsed CNF to internal representation.
asCNF :: ParseCNF.CNF -> CNF
asCNF (ParseCNF.CNF v c is) =
    CNF {numVars = v
        ,numClauses = c
        ,clauses = Set.fromList . map (map fromIntegral . elems) $ is}


verifyBool :: Solution -> Maybe ResolutionTrace -> CNF -> Bool
verifyBool sol maybeRT formula = isNothing $ verify sol maybeRT formula

