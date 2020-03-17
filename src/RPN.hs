module RPN where

import Prelude hiding (lookup)
import qualified Queue as Q
import qualified Stack as S
import Data.Map.Strict
import Data.Char (isAlphaNum)
--import Data.List (elem)

precedence :: Map Char Int
precedence = insert '@' 5 $! insert '|' 4 $! insert '(' 0 $! empty


lookupDefault :: Ord k => a -> k -> Map k a -> a
lookupDefault y k m = case (lookup k m, y) of
    (Just x, _)  -> x
    (Nothing, y) -> y

get k = lookupDefault 0 k precedence

qOut = Q.empty :: Q.Queue2 Char
sMid = S.empty :: S.Stack0 Char

type Token = Char
type Alphabet = String
type St = S.Stack0 Char
type Qu = Q.Queue2 Char
isTerminal :: Token -> Bool
isTerminal = isAlphaNum

{-# INLINE isTerminalG #-}
isTerminalG :: Token -> Alphabet -> Bool
isTerminalG = elem

isOperator :: Token -> Bool
isOperator c = c == '|' || c == '@'

isStar c = c == '*'

convert :: String -> Qu
convert str = conv str sMid qOut
    where conv :: String -> St-> Qu -> Qu
          conv "" s q | S.isEmpty s = q
                      | otherwise = sToQ s q
          conv (t:tokens) s q | isTerminal t || isStar t = conv tokens s (Q.snoc q t)
                              | t == '(' = conv tokens (S.push t s) q
                              | t == ')' = let (s', q') = upd1 (s, q)
                                               in conv tokens s' q'
                              | otherwise = let (s', q') = upd2 t (s, q)
                                               in conv tokens (S.push t s') q'

upd2 :: Char -> (St, Qu) -> (St, Qu)
upd2 t (s, q) | not (S.isEmpty s) && (get (S.peek s) >= get t) = upd2 t ((S.pop s), (Q.snoc q (S.peek s)))
              | otherwise = (s, q)

upd1 :: (St, Qu) -> (St, Qu)
upd1 (s, q) | S.peek s == '(' = (S.pop s, q)
            | otherwise = upd1 ((S.pop s), (Q.snoc q (S.peek s)))

sToQ :: St -> Qu -> Qu
sToQ s q | S.isEmpty s = q
         | otherwise = sToQ (S.pop s) (Q.snoc q (S.peek s))


toStr :: Qu -> String
toStr q | q == Q.empty = ""
        | otherwise = (Q.head q) : toStr (Q.tail q)
