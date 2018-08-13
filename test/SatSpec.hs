module SatSpec where

import Sat
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Data.List (sort)
import Data.Maybe (isNothing)

spec :: Spec
spec = do
    describe "cnfParser" $ do
        it "parses" $ do
            parse cnfParser "" "p cnf 3 2\n1 -3 0\n2 3 -1 0\n" `shouldBe` Right (3, CnfInstance [[1, -3], [2, 3, -1]])

        it "parses" $ do
            parse cnfParser "" "c comment\np cnf 3 2\n1 -3 0\n2 3 -1 0\n" `shouldBe` Right (3, CnfInstance [[1, -3], [2, 3, -1]])

    describe "solve" $ do
        it "returns an assignment for every variable" $ do
            forAll (instGen numVars numLits numClauses) (solveLengthProperty numVars)

        it "finds satisfying solutions" $ do
            forAll (instGen numVars numLits numClauses) (solveSatProperty numVars)

    describe "formatAssignment" $ do
        it "formats unsat result" $ do
            formatAssignment Nothing `shouldBe` "UNSAT"

        it "formats sat result" $ do
            formatAssignment (Just [(1, True), (2, False)]) `shouldBe` "SAT\n1 -2 0"

numVars :: Int
numVars = 5

numClauses :: Int
numClauses = 60

numLits :: Int
numLits = 6

instGen :: Int -> Int -> Int -> Gen CnfInstance
instGen nVars nLits nClauses = CnfInstance <$>
    (resize nClauses $ listOf1 $ resize nLits $ listOf1 $
        oneof [choose (1, nVars), choose (-1, -nVars)])

solveSatProperty :: Int -> CnfInstance -> Property
solveSatProperty nVars inst = let clauses = transform inst
                                  soln = solve nVars clauses
    in classify (isNothing soln) "unsat" $ property $
        case soln of
            Nothing -> True
            Just s -> foldr assign' (Just clauses) s == Just []
    where assign' a = (>>= (assign a))

solveLengthProperty :: Int -> CnfInstance -> Property
solveLengthProperty nVars inst = let clauses = transform inst
                                     soln = solve nVars clauses
    in classify (isNothing soln) "unsat" $ property $
        case soln of
            Nothing -> True
            Just s -> sort (fst <$> s) == [1..nVars]
