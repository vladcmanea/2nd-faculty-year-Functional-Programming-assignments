{- Defini�i �n limbajul Haskell un tip de date TreeBin asociat arborilor binari de c�utare. Defini�i func�ii care s� realizeze c�utarea �ntr-un astfel de arbore, verificarea propriet��ii de arbore binar de c�utare, �i respectiv parcurgerea �n inordine. -}

data TreeBin = Null | Node Int TreeBin TreeBin deriving (Show)

search :: TreeBin -> Int -> Bool
search Null _ = False
search (Node x lt rt) v | v < x = search lt v
  			| v > x = search rt v
			| otherwise = True

check :: TreeBin -> Bool
check Null = True
check (Node x lt rt) = (maxi lt) < x && x < (mini rt) && check lt && check rt

maxi :: TreeBin -> Int
maxi Null = -1000
maxi (Node x lt rt) = max x (maxi rt)

mini :: TreeBin -> Int
mini Null = 1000
mini (Node x lt rt) = min x (mini lt)

inOrder :: TreeBin -> [Int]
inOrder Null = []
inOrder (Node x lt rt) = inOrder lt ++ [x] ++ inOrder rt

{- Defini�i �n limbajul Haskell tipul de date TreeAB asociat arborilor binari cu informa�ii at�t �n nodurile interioare c�t �i �n frunze, cu proprietatea c� informa�iile din nodurile interioare vor fi de tip diferit fa�� de cele din frunze. Defini�i o func�ie mapTreeAB care aplic� dou� func�ii f respectiv g (transmise ca parametru) peste un arbore dat, astfel �nc�t f va modifica valorile din nodurile interioare iar g pe cele din frunze. -}

data TreeX a b = LeafX a | NodeX b (TreeX a b) (TreeX a b) deriving (Show)

mapTree :: (TreeX a b) -> (a -> c) -> (b -> d) -> (TreeX c d)
mapTree (LeafX x) f g = LeafX (f x)
mapTree (NodeX x lt rt) f g = NodeX (g x) (mapTree lt f g) (mapTree rt f g)

--  mapTree (NodeX 'k' (LeafX 3) (LeafX 5)) (\x -> 'a') (\x -> 7)

{- Defini�i �n limbajul Haskell un tip de date Matrix asociat matricilor. Defini�i func�ii pentru: verficarea tipului unei matrici transmis� ca parametru (matricea poate fi p�tratic� sau nu), construirea unei matrici p�tratice pe baza unei liste de valori, adunarea matricilor p�tratice. -}

type Matrix a = [[a]]

isSquare :: Matrix a -> Bool
isSquare xs | length xs == 0 = True
isSquare xs | otherwise = length xs == length (head xs) && and (map (\(x, y) -> x == y) (zip (map (\x -> length x) xs) (tail (map (\x -> length x) xs)))) 

construct :: [a] -> Matrix a
construct xs 	| length xs == 0 = [[]]
		| length [x | x <- [1..(length xs)], x * x == length xs] == 0 = error "List must have a perfect square number of elements :)"
		| otherwise = makeSquare xs (head [x | x <- [1..(length xs)], x * x == length xs])

makeSquare :: [a] -> Int -> Matrix a
makeSquare xs n | length xs == n = [xs]
		| otherwise = [take n xs] ++ makeSquare (drop n xs) n

add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add xs ys 	| isSquare xs == False || isSquare ys == False || length xs /= length ys = [[]]
		| otherwise = map (\(us, vs) -> map (\(x, y) -> x + y) (zip us vs)) (zip xs ys)
		

