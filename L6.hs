{- Definiþi în limbajul Haskell un tip de date TreeBin asociat arborilor binari de cãutare. Definiþi funcþii care sã realizeze cãutarea într-un astfel de arbore, verificarea proprietãþii de arbore binar de cãutare, ºi respectiv parcurgerea în inordine. -}

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

{- Definiþi în limbajul Haskell tipul de date TreeAB asociat arborilor binari cu informaþii atât în nodurile interioare cât ºi în frunze, cu proprietatea cã informaþiile din nodurile interioare vor fi de tip diferit faþã de cele din frunze. Definiþi o funcþie mapTreeAB care aplicã douã funcþii f respectiv g (transmise ca parametru) peste un arbore dat, astfel încât f va modifica valorile din nodurile interioare iar g pe cele din frunze. -}

data TreeX a b = LeafX a | NodeX b (TreeX a b) (TreeX a b) deriving (Show)

mapTree :: (TreeX a b) -> (a -> c) -> (b -> d) -> (TreeX c d)
mapTree (LeafX x) f g = LeafX (f x)
mapTree (NodeX x lt rt) f g = NodeX (g x) (mapTree lt f g) (mapTree rt f g)

--  mapTree (NodeX 'k' (LeafX 3) (LeafX 5)) (\x -> 'a') (\x -> 7)

{- Definiþi în limbajul Haskell un tip de date Matrix asociat matricilor. Definiþi funcþii pentru: verficarea tipului unei matrici transmisã ca parametru (matricea poate fi pãtraticã sau nu), construirea unei matrici pãtratice pe baza unei liste de valori, adunarea matricilor pãtratice. -}

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
		

