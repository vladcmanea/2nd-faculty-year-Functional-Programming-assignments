data Ord a => TreeBin a = Empty | Leaf a | Fork (TreeBin a) a (TreeBin a) deriving Show

mirror :: (Ord a) => TreeBin a -> TreeBin a

mirror Empty = Empty
mirror (Leaf x) = (Leaf x)
mirror (Fork lt x rt) = Fork (mirror rt) x (mirror lt)