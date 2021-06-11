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
      if c >= length cont || all isSpace (drop c cont) then [] else (tk, r, startCol) : lexline' r nextCol cont
      where
        (tk, startCol, nextCol) = case matchLongestToken c cont of
          -- We want to si + 1 because we were 0-index
          Just (tkk, si, nc) -> (tkk, si + 1, nc)
          Nothing -> error "Fail to tokenize"

-- Matches the longest token starting from column
-- Returns Maybe (Token, StartIdx, NextCol) due to possible failure to match
matchLongestToken :: Int -> String -> Maybe (Token, Int, Int)
matchLongestToken startIdx content
  | "bool" `isPrefixOf` stripped = create TkBool "bool"
  | "break" `isPrefixOf` stripped = create TkBreak "break"
  | "import" `isPrefixOf` stripped = create TkImport "import"
  | "continue" `isPrefixOf` stripped = create TkContinue "continue"
  | "else" `isPrefixOf` stripped = create TkElse "else"
  | "false" `isPrefixOf` stripped = create TkFalse "false"
  | "for" `isPrefixOf` stripped = create TkFor "for"
  | "while" `isPrefixOf` stripped = create TkWhile "while"
  | "if" `isPrefixOf` stripped =  create TkIf "if"
  | "int" `isPrefixOf` stripped = create TkInt "int"
  | "return" `isPrefixOf` stripped = create TkReturn "return"
  | "len" `isPrefixOf` stripped = create TkLen "len"
  | "true" `isPrefixOf` stripped = create TkTrue "true"
  | "void" `isPrefixOf` stripped = create TkVoid "void"
  | otherwise = Nothing -- Possibly Either as one is probably an error, and one could be something where it does not happen
  where
    (stripped, amountStripped) = lstrip (drop startIdx content)
    idxAfterStripped = startIdx + amountStripped
    create :: Token -> String -> Maybe (Token, Int, Int)
    create token str = Just (token, idxAfterStripped, idxAfterStripped + length str)
