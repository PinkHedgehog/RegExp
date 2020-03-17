data BinTree a = Null | Node a (BinTree a) (BinTree a)

insert :: Ord a => a -> BinTree a -> BinTree a
insert x Null = Node x Null Null
insert x n@(Node y left right) | x < y = Node y (insert x left) right
                               | x > y = Node y left (insert x right)
                               | otherwise = n

in_order :: Show a => BinTree a -> String
in_order Null = ""
in_order (Node x left right) = in_order left ++ " " ++ show x ++ in_order right

fromList :: Ord a => [a] -> BinTree a
fromList [] = Null
fromList (x:xs) = insert x (fromList xs)
