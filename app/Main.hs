module Main where

import Helpers
import Data.List
import Data.Char (isSpace)

import Debug.Trace


data Token = TkIf               -- Keywords begin
            | TkBool
            | TkBreak
            | TkImport
            | TkContinue
            | TkElse
            | TkFalse
            | TkFor
            | TkWhile
            | TkInt
            | TkReturn
            | TkLen
            | TkTrue
            | TkVoid            -- Keywords end
            | TkStrLiteral String
            | TkCharLiteral String
            | TkIntLiteral String
            | TkHexLiteral String
            | TkIdentifier String
            | TkSemicolon       -- Special symbols
            | TkComma
            | TkOpenBracket     -- Bracket: [], Braces: {}, Paren: ()
            | TkOpenBraces
            | TkOpenParen
            | TkCloseBracket
            | TkCloseBraces
            | TkCloseParen
            | TkPlus
            | TkMinus
            | TkExclamation
            | TkAsterisk
            | TkFSlash
            | TkPercent
            | TkEqual
            | TkLess
            | TkGreater
            | TkLessEqual
            | TkGreaterEqual
            | TkEqualEqual
            | TkNotEqual
            | TkPlusEqual
            | TkMinusEqual
            | TkAnd
            | TkOr
            deriving (Eq, Show)

-- TODO pick file to read or read from stdin
main :: IO ()
main = do
       inp <- readFile "lexthis.txt"
       print inp
       (print . genTokens) inp

genTokens :: String -> [(Token, Int, Int)]
genTokens = lexlines . lines

-- Takes a list of lines and then outputs tokens w/ row + col
lexlines :: [String] -> [(Token, Int, Int)]
lexlines [] = []
lexlines lns = lexlines' res
  where
    -- First zips lines with line number and then filter lines that are comments
    res = filter (\(_,x) -> not ("//" `isPrefixOf` dropWhile isSpace x)) (zip [1..] lns)
    lexlines' :: [(Int, String)] -> [(Token, Int, Int)]
    lexlines' [] = []
    lexlines' (x:xs) = lexline x ++ lexlines' xs

lexline :: (Int, String) -> [(Token, Int, Int)]
lexline (row, content) = lexline' row 0 content
  where
    lexline' :: Int -> Int -> String -> [(Token, Int, Int)]
    lexline' r c cont = 
      if c >= length cont || all isSpace (drop c cont) then [] else (tk, r, c) : lexline' r nextCol cont
      where
        (tk, nextCol) = case matchLongestToken c cont of
          Just (tkk, nc) -> (tkk, nc)
          Nothing -> error "Fail to tokenize"

-- Matches the longest token starting from column
-- Returns Maybe due to possible failure to match
matchLongestToken :: Int -> String -> Maybe (Token, Int)
matchLongestToken startIdx content
  | "bool" `isPrefixOf` stripped = Just (TkBool, idxAfterStripped + 4)
  | "break" `isPrefixOf` stripped = Just (TkBreak, idxAfterStripped + 5)
  | "import" `isPrefixOf` stripped = Just (TkImport, idxAfterStripped + 6)
  | "continue" `isPrefixOf` stripped = Just (TkContinue, idxAfterStripped + 8)
  | "else" `isPrefixOf` stripped = Just (TkElse, idxAfterStripped + 4)
  | "false" `isPrefixOf` stripped = Just (TkFalse, idxAfterStripped + 5)
  | "for" `isPrefixOf` stripped = Just (TkFor, idxAfterStripped + 3)
  | "while" `isPrefixOf` stripped = Just (TkWhile, idxAfterStripped + 5)
  | "if" `isPrefixOf` stripped =  Just (TkIf, idxAfterStripped + 2)
  | "int" `isPrefixOf` stripped = Just (TkInt, idxAfterStripped + 3)
  | "return" `isPrefixOf` stripped = Just (TkReturn, idxAfterStripped + 6)
  | "len" `isPrefixOf` stripped = Just (TkLen, idxAfterStripped + 3)
  | "true" `isPrefixOf` stripped = Just (TkTrue, idxAfterStripped + 4)
  | "void" `isPrefixOf` stripped = Just (TkVoid, idxAfterStripped + 4)
  | otherwise = Nothing -- Possibly Either as one is probably an error, and one could be something where it does not happen
  where
    (stripped, amountStripped) = lstrip (drop startIdx content)
    idxAfterStripped = startIdx + amountStripped
