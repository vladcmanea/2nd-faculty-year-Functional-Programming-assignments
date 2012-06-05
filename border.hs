data Ord a => Tree a = Leaf a | Tree [Tree a] deriving Show

border :: Ord a => Tree a -> a -> a -> [Tree a]
border (Leaf v) l r 	| l <= v && v <= r = [Leaf v]
border (Leaf v) l r	| otherwise = []
border (Tree xs) l r	| True = foldr (++) [] (map (\x -> border x l r) xs)

tree = Tree [Tree [Leaf 3, Tree [Leaf 4]], Tree [Leaf 5], Tree [Leaf 7]]