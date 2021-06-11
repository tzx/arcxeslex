module Helpers where

import Data.Char (isSpace)
import Debug.Trace

-- Stripes whitespace and gives amount stripped 
lstrip :: String -> (String, Int)
lstrip str = lstrip' (str, 0)
  where
    lstrip' :: (String, Int) -> (String, Int)
    lstrip' ("",c) = ("",c)
    lstrip' (x:xs, c) = if isSpace x then lstrip' (xs, c + 1) else (x:xs, c)
