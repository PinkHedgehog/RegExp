module Queue where

class Queue q where
    empty :: q a
    head  :: q a -> a
    tail  :: q a -> q a
    snoc  :: q a -> a -> q a


newtype Queue0 a = Q0 [a]

data StrictList a = SNil | SCons a !(StrictList a) deriving (Show, Eq)


instance Queue Queue0 where
    empty = Q0 []
    head (Q0 (x:_)) = x
    tail (Q0 (_:xs)) = Q0 xs
    snoc (Q0 xs) x = Q0 (xs ++ [x])


app :: StrictList a -> StrictList a -> StrictList a
app SNil ys = ys
app (SCons x xs) ys = SCons x (app xs ys)

rev :: StrictList a -> StrictList a
rev = go SNil
    where go :: StrictList a -> StrictList a -> StrictList a
          go acc SNil = acc
          go acc (SCons x xs) = go (SCons x acc) xs

data Queue1 a = Q1 !Int !(StrictList a) !Int !(StrictList a) deriving (Show, Eq)

inv1 :: Queue1 a -> Queue1 a
inv1 q@(Q1 f xs r ys)
    | f < r = Q1 (f + r) (xs `app` rev ys) 0 SNil
    | otherwise = q

instance Queue Queue1 where
    empty                            = Q1 0 SNil 0 SNil
    head (Q1 _ (SCons x _  ) _ _ )   = x
    tail (Q1 f (SCons _ xs ) r ys)   = inv1 $ Q1 (f - 1) xs r ys
    snoc (Q1 f xs            r ys) y = inv1 $ Q1 f xs (r + 1) (SCons y ys)

data Queue2 a = Q2 !Int [a] !Int !(StrictList a) deriving (Show, Eq)

rev' :: StrictList a -> [a]
rev' = go []
    where go :: [a] -> StrictList a -> [a]
          go acc SNil = acc
          go acc (SCons x xs) = go (x:acc) xs

inv2 :: Queue2 a -> Queue2 a
inv2 q@(Q2 f xs r ys)
    | f < r     = Q2 (f+r) (xs ++ rev' ys) 0 SNil
    | otherwise = q

instance Queue Queue2 where
    empty                     = Q2 0 [] 0 SNil
    head (Q2 _ (x:_ ) _ _ )   = x
    tail (Q2 f (_:xs) r ys)   = inv2 $ Q2 (f-1) xs r ys
    snoc (Q2 f xs     r ys) y = inv2 $ Q2 f xs (r+1) (SCons y ys)
