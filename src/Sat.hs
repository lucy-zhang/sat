module Sat where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (mplus)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

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

transform :: CnfInstance -> [IntMap Bool]
transform (CnfInstance clauses) = fmap (M.fromList . fmap lit) clauses
    where lit n = if n > 0 then (n, True) else (-n, False)

assign :: (Int, Bool) -> [IntMap Bool] -> Maybe [IntMap Bool]
assign assignment clauses
    | any M.null newClauses = Nothing
    | otherwise = Just newClauses
    where newClauses = mapMaybe (assignInClause assignment) clauses
          assignInClause (n, b) cl = case M.lookup n cl of
            Nothing -> Just cl
            Just b'
                | b == b' -> Nothing
                | otherwise -> Just $ M.delete n cl

solve :: [IntMap Bool] -> Maybe [(Int, Bool)]
solve clauses = solve' [] clauses

solve' :: [(Int, Bool)] -> [IntMap Bool] -> Maybe [(Int, Bool)]
solve' assigns [] = Just assigns
solve' assigns clauses = case (unitAssignment clauses, pureAssignment clauses) of
        (Just a, _) -> (assign a clauses) >>= solve' (a : assigns)
        (_, Just a) -> (assign a clauses) >>= solve' (a : assigns)
        _ -> let (n, _) = M.findMin $ head clauses
                 trueCase = (assign (n, True) clauses) >>= solve' ((n, True) : assigns)
                 falseCase = (assign (n, False) clauses) >>= solve' ((n, False) : assigns)
            in mplus trueCase falseCase

unitAssignment :: [IntMap Bool] -> Maybe (Int, Bool)
unitAssignment cls = case filter ((== 1) . M.size) cls of
    (c : _) -> Just $ head $ M.toList c
    _ -> Nothing

pureAssignment :: [IntMap Bool] -> Maybe (Int, Bool)
pureAssignment clauses = M.lookupMin pureVars
    where pureVars = M.mapMaybe id $ M.unionsWith
            (\m1 -> \m2 -> case (m1, m2) of
                (Just b1, Just b2) | b1 == b2 -> Just b1
                _ -> Nothing)
            ((M.map Just) <$> clauses)

formatAssignment :: Maybe [(Int, Bool)] -> String
formatAssignment Nothing = "UNSAT"
formatAssignment (Just as) = "SAT\n" <>
    (unwords $ (map (\(i, b) -> show $ (if b then id else negate) i) $ sort as) <> ["0"])
