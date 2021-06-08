module Main where

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

main :: IO ()
main = putStrLn "Hello, Haskell!"
