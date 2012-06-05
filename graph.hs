type Node a = (Int, a)
type Edge = (Int, Int)
data Graph a = G [Node a] [Edge]
  deriving Show

{- ia vecinii unui nod din graf -}
neighbors :: Graph a -> Int -> [Node a]
neighbors (G ns es) i = [(x, y) | (x, y) <- ns, length ([(u, v) | (u, v) <- es, u == x, v == i]) + length ([(u, v) | (u, v) <- es, u == i, v == x]) > 0]
