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
            parse cnfParser "" "p cnf 3 2\n1 -3 0\n2 3 -1 0\n" `shouldBe` Right (CnfInstance [[1, -3], [2, 3, -1]])

    describe "cnfToExpr" $ do
        it "converts to Expr" $ do
            cnfToExpr (CnfInstance [[1, -3], [2, 3, -1]]) `shouldBe` And (Or (Var 1) (Not (Var 3))) (Or (Var 2) (Or (Var 3) (Not (Var 1))))

        it "handles a single clause" $ do
            cnfToExpr (CnfInstance [[1, 2]]) `shouldBe` Or (Var 1) (Var 2)

        it "handles a clause with one literal" $ do
            cnfToExpr (CnfInstance [[1], [2]]) `shouldBe` And (Var 1) (Var 2)

        it "handles just a literal" $ do
            cnfToExpr (CnfInstance [[1]]) `shouldBe` Var 1

    describe "simplify" $ do
        it "true should simplify to true" $ do
            simplify (Const True) `shouldBe` (Const True)

        it "simplifies variables to themselves" $ do
            simplify (Var 1) `shouldBe` (Var 1)

        it "simplifies not of a variable" $ do
            simplify (Not (Var 1)) `shouldBe` Not (Var 1)

        it "simplifies not of a constant" $ do
            simplify (Not (Const True)) `shouldBe` Const False

        it "simplifies not of not" $ do
            simplify (Not (Not (Const True))) `shouldBe` Const True

        it "simplifies and with const true" $ do
            simplify (And (Const True) (Var 1)) `shouldBe` (Var 1)

        it "simplifies and with const false" $ do
            simplify (And (Const False) (Var 1)) `shouldBe` (Const False)

        it "simplifies and with const false recursively" $ do
            simplify (And (Not (Const True)) (Var 1)) `shouldBe` (Const False)

        it "simplifies or with const true" $ do
            simplify (Or (Const True) (Var 1)) `shouldBe` (Const True)

        it "simplifies or with const false" $ do
            simplify (Or (Const False) (Var 1)) `shouldBe` (Var 1)

        it "simplifies or with const true recursively" $ do
            simplify (Or (Not (Const False)) (Var 1)) `shouldBe` (Const True)

    describe "assign" $ do
        it "assigns to a variable" $ do
            assign (1, True) (Var 1) `shouldBe` Const True

        it "assigns to a different variable" $ do
            assign (1, True) (Var 2) `shouldBe` Var 2

        it "assigns recursively" $ do
            assign (1, True) (And (Var 1) (Var 2)) `shouldBe` And (Const True) (Var 2)

    describe "solve" $ do
        it "returns an assignment for every variable" $ do
            forAll (instGen numVars numLits numClauses) (solveLengthProperty numVars)

        it "finds satisfying solutions" $ do
            forAll (instGen numVars numLits numClauses) (solveSatProperty numVars)

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
solveSatProperty nVars inst = let expr = cnfToExpr inst
                                  soln = solve nVars expr
    in classify (isNothing soln) "unsat" $ property $
        case soln of
            Nothing -> True
            Just s -> simplify (foldr assign expr s) == Const True

solveLengthProperty :: Int -> CnfInstance -> Property
solveLengthProperty nVars inst = let expr = cnfToExpr inst
                                     soln = solve nVars expr
    in classify (isNothing soln) "unsat" $ property $
        case soln of
            Nothing -> True
            Just s -> sort (fst <$> s) == [1..nVars]
