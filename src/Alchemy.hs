module Alchemy where

import qualified Data.Group.Free as FG
import           Data.Algebra.Free
import           Data.Char
import           Data.Group


interpret :: [Char] -> FG.FreeGroupL Char
interpret = foldMap inject          -- that's `foldMap` from Data.Foldable

inject :: Char -> FG.FreeGroupL Char
inject c
    | isAlpha c && isLower c = returnFree c
    | isAlpha c && isUpper c = invert (returnFree (toLower c))
    | otherwise              = mempty       -- group identity element

day05a :: [Char] -> Int
day05a = length . FG.toList . foldMap inject

clean
    :: Char                                     -- ^ given a letter to clean
    -> (FG.FreeGroupL Char -> FG.FreeGroupL Char)     -- ^ return a group homomorphism
clean c = foldMapFree $ \d ->
        if d == c
            then mempty
            else returnFree d

day05b :: String -> Int
day05b rawInput = minimum
    [ length (FG.toList (clean c polymer))
    | c <- ['a' .. 'z']
    ]
  where
    polymer :: FG.FreeGroupL Char
    polymer = foldMap inject rawInput
