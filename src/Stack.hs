module Stack where

class Stack s where
    empty :: s a
    peek  :: s a -> a
    pop   :: s a -> s a
    push  :: a -> s a -> s a
    isEmpty :: s a -> Bool
    --peekWhile :: s a -> (a -> Bool) -> [a]
    --popWhile  :: s a -> (a -> Bool) -> s a
newtype Stack0 a = S0 [a] deriving (Show, Eq)

instance Stack Stack0 where
    empty           = S0 []
    peek (S0 [])    = error "Empty stack!"
    peek (S0 (x:_)) = x
    pop  (S0 [])    = S0 []
    pop (S0 (x:xs)) = S0 xs
    push x (S0 xs)  = S0 $! (:) x $! xs
    isEmpty (S0 xs) = null xs
    --peekWhile s p | isEmpty s = []
