module SatSpec where

import Sat
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Data.Maybe (isNothing)
import qualified Data.IntMap.Strict as M

spec :: Spec
spec = do
    describe "cnfParser" $ do
        it "parses" $ do
            parse cnfParser "" "p cnf 3 2\n1 -3 0\n2 3 -1 0\n" `shouldBe` Right (3, CnfInstance [[1, -3], [2, 3, -1]])

        it "parses" $ do
            parse cnfParser "" "c comment\np cnf 3 2\n1 -3 0\n2 3 -1 0\n" `shouldBe` Right (3, CnfInstance [[1, -3], [2, 3, -1]])

    describe "unitAssignment" $ do
        it "finds unit clauses" $ do
            unitAssignment [M.singleton 1 True] `shouldBe` Just (1, True)

        it "finds the first unit clause" $ do
            unitAssignment [M.singleton 1 True, M.singleton 2 False] `shouldBe` Just (1, True)

        it "finds the unit clause when there are other clauses" $ do
            unitAssignment [M.fromList [(2, True), (3, False)], M.singleton 1 True] `shouldBe` Just (1, True)

        it "returns Nothing if there are no unit clauses" $ do
            unitAssignment [M.fromList [(2, True), (3, False)]] `shouldBe` Nothing

    describe "pureAssignment" $ do
        it "finds a variable that only occurs once" $ do
            pureAssignment [M.fromList [(2, True), (3, False)]] `shouldBe` Just (2, True)

        it "finds a variable that appears pure in two clauses" $ do
            pureAssignment [M.fromList [(2, True), (3, False)], M.fromList [(2, True), (3, True)]] `shouldBe` Just (2, True)

        it "finds a variable that appears pure in two clauses (negated)" $ do
            pureAssignment [M.fromList [(2, False), (3, False)], M.fromList [(2, False), (3, True)]] `shouldBe` Just (2, False)

        it "returns Nothing if no variable appears pure" $ do
            pureAssignment [M.fromList [(2, True), (3, False)], M.fromList [(2, False), (3, True)]] `shouldBe` Nothing

    describe "solve" $ do
        it "finds satisfying solutions" $ do
            forAll (instGen numVars numLits numClauses) solveSatProperty

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

solveSatProperty :: CnfInstance -> Property
solveSatProperty inst = let clauses = transform inst
                            soln = solve clauses
    in classify (isNothing soln) "unsat" $ property $
        case soln of
            Nothing -> True
            Just s -> foldr assign' (Just clauses) s == Just []
    where assign' a = (>>= (assign a))
