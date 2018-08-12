module Sat where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (mplus)
import Data.List (sort)

newtype CnfInstance = CnfInstance { vars :: [[Int]] }
    deriving (Eq, Show)

cnfParser :: Parser (Int, CnfInstance)
cnfParser = do
    _ <- many comment
    nVars <- problem
    _ <- many comment
    clauses <- endBy1 clause newline
    return (nVars, CnfInstance clauses)

clause :: Parser [Int]
clause = init <$> sepBy1 int (char ' ')
    where int = (char '-' >> negate <$> int) <|> (readInt <$> many1 digit)
          readInt = read :: String -> Int

comment :: Parser String
comment = string "c " *> many (noneOf "\n") <* newline

problem :: Parser Int
problem = string "p cnf " *> nat <* char ' ' <* nat <* spaces
    where nat = (read :: String -> Int) <$> many1 digit

parseCnf :: String -> Either ParseError (Int, CnfInstance)
parseCnf = parse cnfParser ""

data Expr
    = And Expr Expr
    | Or Expr Expr
    | Not Expr
    | Var Int
    | Const Bool
    deriving (Eq, Show)

cnfToExpr :: CnfInstance -> Expr
cnfToExpr (CnfInstance x) = foldr1 And $ fmap (foldr1 Or) $ (fmap . fmap) toAtom x
    where toAtom n
            | n > 0 = Var n
            | otherwise = Not (Var (-n))

simplify :: Expr -> Expr
simplify (Not e) = case simplify e of
    Const b -> Const (not b)
    x -> Not x
simplify (And e1 e2) = case (simplify e1, simplify e2) of
    (Const True, e) -> e
    (Const False, _) -> Const False
    (e, Const True) -> e
    (_, Const False) -> Const False
    (x, y) -> And x y
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
    (Const False, e) -> e
    (Const True, _) -> Const True
    (e, Const False) -> e
    (_, Const True) -> Const True
    (x, y) -> Or x y
simplify x = x

assign :: (Int, Bool) -> Expr -> Expr
assign _ x@(Const _) = x
assign (n, b) (Var n')
    | n == n' = Const b
    | otherwise = Var n'
assign a (Not e) = Not (assign a e)
assign a (And e1 e2) = And (assign a e1) (assign a e2)
assign a (Or e1 e2) = Or (assign a e1) (assign a e2)

solve :: Int -> Expr -> Maybe [(Int, Bool)]
solve maxVar expr = solve' [] 1 maxVar expr
    where solve' as minVar' maxVar' (Const True) = Just $ ((\n -> (n, True)) <$> [maxVar', maxVar'-1..minVar']) ++ as
          solve' _ _ _ (Const False) = Nothing
          solve' as minVar' maxVar' e =
            let true = solve' ((minVar', True) : as) (minVar' + 1) maxVar' (simplify (assign (minVar', True) e))
                false = solve' ((minVar', False) : as) (minVar' + 1) maxVar' (simplify (assign (minVar', False) e))
            in true `mplus` false

formatAssignment :: Maybe [(Int, Bool)] -> String
formatAssignment Nothing = "UNSAT"
formatAssignment (Just as) = "SAT\n" <>
    (unwords $ (map (\(i, b) -> show $ (if b then id else negate) i) $ sort as) <> ["0"])
