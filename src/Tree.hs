module Tree where

import Stack
import Data.Char (isAlphaNum)
import RPN



data Tree = Literal Char
          | Union Tree Tree
          | Concat Tree Tree
          | Star Tree
          | Epsilon
          deriving (Show, Eq)

-- instance Show Tree where
--     show Epsilon = "~"
--     show (Literal c) = "{"++[c]++"}\n"
--     show (Union l r) = "Union (" ++ show l ++ " | " ++ show r ++ " )\n"
--     show (Concat l r) = "Concat (" ++ show l ++ " | " ++ show r ++ " )\n"
--     show (Star e) = "Star " ++ show e ++ "\n"

(.>>) = flip ($)

-- bracketize :: String -> [(Char, Word32)]
-- bracketize str = zip str [1..len] .>> filter (isBracket . fst)
--     where len = length str .>> fromIntegral
--           isBracket c = (c == '(') || (c == ')')

doggy :: String -> String
doggy "" = ""
doggy [c] = [c]
doggy (x:y:xs) | (isAlphaNum x || x == ')' || x == '*') && (isAlphaNum y || y == '(') || y == '#' = x : '@' : doggy (y:xs)
               | otherwise = x : doggy (y:xs)


endingHash :: String -> String
endingHash str
    | last str == '#' = str
    | otherwise = str ++ "#"
