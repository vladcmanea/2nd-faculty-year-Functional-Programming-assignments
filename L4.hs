{- Definiþi funcþia isDescending, care determinã dacã o listã datã este ordonatã descrescãtor. Pentru definiþia funcþiei isDescending se vor utiliza funcþia de bibliotecã zip (a se revedea exemplul discutat la laborator) ºi mecanismul de list comprehension. -}

isDescending :: (Ord a) => [a] -> Bool
isDescending xs = and (map (\(x, y) -> x >= y) [(a, b) | (a, b) <- zip xs (tail xs)])

{- Definiþi funcþia _length :: [a] -> Int care calculeazã lungimea unei liste, utilizând foldr. -}

_length :: [a] -> Int
_length xs = foldr (\x y -> 1 + y) 0 xs

{- Definiþi funcþia _reverse [a] -> [a] care calculeazã oglinditul unei liste, utilizând foldl. -}

_reverse :: [a] -> [a]
_reverse xs = foldl (\x y -> [y] ++ x) [] xs

{- Definiþi funcþia detNmb :: [Int] -> Int care transformã o listã de cifre în numãrul întreg asociat, utilizând foldl. -}

detNmb :: [Int] -> Int
detNmb xs = foldl (\x y -> x * 10 + y) 0 xs