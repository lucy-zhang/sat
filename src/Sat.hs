module Sat where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (mplus)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M

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

transform :: CnfInstance -> [Map Int Bool]
transform (CnfInstance clauses) = fmap (M.fromList . fmap lit) clauses
    where lit n = if n > 0 then (n, True) else (-n, False)

assign :: (Int, Bool) -> [Map Int Bool] -> Maybe [Map Int Bool]
assign assignment clauses
    | any M.null newClauses = Nothing
    | otherwise = Just newClauses
    where newClauses = mapMaybe (assignInClause assignment) clauses
          assignInClause (n, b) cl = case M.lookup n cl of
            Nothing -> Just cl
            Just b'
                | b == b' -> Nothing
                | otherwise -> Just $ M.delete n cl

solve :: Int -> [Map Int Bool] -> Maybe [(Int, Bool)]
solve maxVar clauses = solve' [] 1 maxVar (Just clauses)
    where solve' _ _ _ Nothing = Nothing
          solve' as minVar' maxVar' (Just []) =
              Just $ ((\n -> (n, True)) <$> [maxVar', maxVar'-1..minVar']) ++ as
          solve' as minVar' maxVar' (Just cs) =
            let true = solve' ((minVar', True) : as) (minVar' + 1) maxVar' (assign (minVar', True) cs)
                false = solve' ((minVar', False) : as) (minVar' + 1) maxVar' (assign (minVar', False) cs)
            in true `mplus` false

formatAssignment :: Maybe [(Int, Bool)] -> String
formatAssignment Nothing = "UNSAT"
formatAssignment (Just as) = "SAT\n" <>
    (unwords $ (map (\(i, b) -> show $ (if b then id else negate) i) $ sort as) <> ["0"])
