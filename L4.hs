{- Defini�i func�ia isDescending, care determin� dac� o list� dat� este ordonat� descresc�tor. Pentru defini�ia func�iei isDescending se vor utiliza func�ia de bibliotec� zip (a se revedea exemplul discutat la laborator) �i mecanismul de list comprehension. -}

isDescending :: (Ord a) => [a] -> Bool
isDescending xs = and (map (\(x, y) -> x >= y) [(a, b) | (a, b) <- zip xs (tail xs)])

{- Defini�i func�ia _length :: [a] -> Int care calculeaz� lungimea unei liste, utiliz�nd foldr. -}

_length :: [a] -> Int
_length xs = foldr (\x y -> 1 + y) 0 xs

{- Defini�i func�ia _reverse [a] -> [a] care calculeaz� oglinditul unei liste, utiliz�nd foldl. -}

_reverse :: [a] -> [a]
_reverse xs = foldl (\x y -> [y] ++ x) [] xs

{- Defini�i func�ia detNmb :: [Int] -> Int care transform� o list� de cifre �n num�rul �ntreg asociat, utiliz�nd foldl. -}

detNmb :: [Int] -> Int
detNmb xs = foldl (\x y -> x * 10 + y) 0 xs