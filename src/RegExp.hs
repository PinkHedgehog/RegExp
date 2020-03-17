{-# LANGUAGE DeriveFunctor, ApplicativeDo, LambdaCase #-}
module RegExp where

import Control.Alternative.Free
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Tree (Tree(..), Tree, doggy, endingHash)
import Stack as S
import RPN hiding (get)
data Prim a = Prim Char a deriving (Functor, Show)


type RegExp = Alt Prim

-- | charAs: Интерпретирует указанный символ, как некоторую константу
charAs :: Char -> a -> RegExp a
charAs c x = liftAlt (Prim c x)  -- liftAlt :: f a -> Alt f a позволяет использовать
                                 -- базовый функтор Prim в типе RegExp

-- | char: Интерпретирует и возвращает в качестве результата указанный символ
char :: Char -> RegExp Char
char c = charAs c c

-- | string: Интерпретирует и возвращает в качестве результата указанную строку
string :: String -> RegExp String
string = traverse char           -- классно, а?


testRegExp_ :: RegExp ()
testRegExp_ = void $ (char 'a' <|> char 'b')
    *> many (string "cd")
    *> char 'e'

digit :: RegExp Int
digit = asum [ charAs (intToDigit i) i | i <- [0..9] ]

bracketDigit :: RegExp Int
bracketDigit = char '[' *> digit <* char ']'

testRegExp :: RegExp Int
testRegExp = (char 'a' <|> char 'b')
          *> (length <$> many (string "cd"))
          <* char 'e'


testRegExpDo :: RegExp Int
testRegExpDo = do
    char 'a' <|> char 'b'
    cds <- many (string "cd")
    char 'e'
    pure (length cds)


processPrim :: Prim a -> StateT String Maybe a
processPrim (Prim c x) = do
    d:ds <- get
    guard (c == d)
    put ds
    pure x

prepare = toStr . convert . doggy-- . endingHas

getTree :: String -> Tree
getTree str = let s = prepare str
                  in S.peek $! f s (S.empty :: S.Stack0 Tree)
    where
        f :: String -> S.Stack0 Tree -> S.Stack0 Tree
        f [] s = s
        f (t:tokens) st | t == '~' = f tokens $ S.push Epsilon st
                        | isTerminal t || isAlphaNum t = f tokens $ S.push (Literal t) st
                        | t == '*' = let (ex, s) = (S.peek st, S.pop st)
                                         in f tokens $ S.push (Star ex) s
                        | t == '@' = let (ex1, st1) = (S.peek st, S.pop st)
                                         (ex2, st2) = (S.peek st1, S.pop st1)
                                         in f tokens (S.push (Concat ex2 ex1) st2)
                        | t == '|' = let (ex1, st1) = (S.peek st, S.pop st)
                                         (ex2, st2) = (S.peek st1, S.pop st1)
                                         in f tokens (S.push (Union ex2 ex1) st2)

-- getReady :: String -> RegExp ()
-- getReady str = let s' = toStr . convert . doggy . endingHash $ str
--                    in S.peek $! f s' (S.empty :: S.Stack0 (RegExp ()))
--     where
--         f :: String -> S.Stack0 (RegExp ()) -> S.Stack0 (RegExp ())
--         f [] s = s
--         f (t:tokens) st | t == '~' = f tokens (S.push Control.Applicative.empty st)
--                         | isTerminal t = f tokens (S.push (void $ char t) st)
--                         | t == '*' = let (ex, newst) = (S.peek st, S.pop st)
--                                          in f tokens (S.push (void $ many ex) newst)
--                         | t == '@' = let (ex1, st1) = (S.peek st, S.pop st)
--                                          (ex2, st2) = (S.peek st, S.pop st1)
--                                          in f tokens (S.push (ex2 *> ex1) st2)
--                         | t == '|' = let (ex1, st1) = (S.peek st, S.pop st)
--                                          (ex2, st2) = (S.peek st, S.pop st1)
--                                          in f tokens (S.push (ex2 <|> ex1) st2)
--                         | otherwise = error "unidentified token!"

tree :: Tree -> RegExp ()
tree Epsilon      = void $ char '~'
tree (Literal c)  = void $ char c
--tree (Concat l (Literal '#')) = tree l
tree (Concat l r) = tree l *> tree r
tree (Union l r)  = tree l <|> tree r
tree (Star e)     = void $ many (tree e)

matchPrefix :: RegExp a -> String -> Maybe a
matchPrefix re = evalStateT (runAlt processPrim re)

matchFull :: RegExp a -> String -> Bool
matchFull re str = case execStateT (runAlt processPrim re) str of
    Just rest -> null rest
    Nothing   -> False

parseRE :: String -> RegExp ()
parseRE = tree . getTree
