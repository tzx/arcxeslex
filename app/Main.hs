module Main where

import Helpers
import Data.List
import Data.Char (isSpace)

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
       -- XD: TODO
       (putStrLn . genTokensString) inp

-- Takes a list of lines and then outputs tokens w/ row + col
lexlines :: [String] -> [(Token, Int, Int)]
lexlines [] = []
lexlines lines = lexlines' res
  where
    -- First zips lines with line number and then filter lines that are comments
    res = filter (\(_,x) -> (negate . isPrefixOf) "//" (dropWhile isSpace x)) (zip [1..] lines)
    lexlines' :: [(Int, String)] -> [(Token, Int, Int)]
    lexlines' [] = []
    lexlines' (x:xs) = lexline x ++ lexlines' xs

lexline :: (Int, String) -> [(Token, Int, Int)]
lexline (row, content) = lexline' row 0 content
  where
    -- TODO: no basecase
    lexline' :: Int -> Int -> String -> [(Token, Int, Int)]
    lexline' row column content = (tk, row, column) : lexline' row nextCol content
      where
        -- TODO: matchLongestToken can fail so we need to check for Nothing
        (tk, nextCol) = matchLongestToken column content


-- Matches the longest token starting from column
-- Returns Maybe due to possible failure to match
matchLongestToken :: Int -> String -> Maybe (Token, Int)
matchLongestToken startIdx content
  | "bool" `isPrefixOf` content = Just (TkBool, idxAfterStripped + 4)
  | "break" `isPrefixOf` content = Just (TkBreak, idxAfterStripped + 5)
  | "import" `isPrefixOf` content = Just (TkImport, idxAfterStripped + 6)
  | "continue" `isPrefixOf` content = Just (TkContinue, idxAfterStripped + 8)
  | "else" `isPrefixOf` content = Just (TkElse, idxAfterStripped + 4)
  | "false" `isPrefixOf` content = Just (TkFalse, idxAfterStripped + 5)
  | "for" `isPrefixOf` content = Just (TkFor, idxAfterStripped + 3)
  | "while" `isPrefixOf` content = Just (TkWhile, idxAfterStripped + 5)
  | "if" `isPrefixOf` content =  Just (TkIf, idxAfterStripped + 2)
  | "int" `isPrefixOf` content = Just (TkInt, idxAfterStripped + 3)
  | "return" `isPrefixOf` content = Just (TkReturn, idxAfterStripped + 6)
  | "len" `isPrefixOf` content = Just (TkLen, idxAfterStripped + 3)
  | "true" `isPrefixOf` content = Just (TkTrue, idxAfterStripped + 4)
  | "void" `isPrefixOf` content = Just (TkVoid, idxAfterStripped + 4)
  | otherwise = Nothing
  where
    (stripped, amountStripped) = lstrip (take startIdx content)
    idxAfterStripped = startIdx + amountStripped
