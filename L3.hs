{- Utiliz�nd conceptul de list comprehension, construi�i o expresie care calculeaz� suma 1*1 + ... + 100 * 100 -}

sumsquares :: (Integral a) => a -> a
sumsquares n 	| n < 0 = error "N too small"
		| otherwise = sum [x * x | x <- [1..n]]

{- O 3-upl� (x, y, z) se nume�te pythagorean dac� x * x + y * y = z * z. Utiliz�nd list comprehension, defini�i func�ia pyths :: Int -> [(Int, Int, Int)] care returneaz� lista tuturor 3-uplelor de form� pythagorean, de o anumit� limit�. -}

pyths :: (Integral a) => a -> [(a, a, a)]
pyths x = [(a, b, c) | c <- [1..x], a <- [1..c], b <- [1..c], a * a + b * b == c * c]

{- Un num�r �ntreg se nume�te perfect dac� este egal cu suma divizorilor s�i, excluz�nd num�rul �nsu�i. Utiliz�nd list comprehension �i func�ia factors, defini�i func�ia perfects :: Int -> [Int] care returneaz� lista tuturor numerelor perfecte, p�n� la o anumit� limit�. -}

perfects :: Int -> [Int]
perfects x = [y | y <- [1..x], y == sum (factors y)]

factors :: Int -> [Int]
factors x = [z | z <- [1..(x `div` 2)], x `mod` z == 0]

{- Ar�ta�i cum lista (dat� cu list comprehension) [f x | x <- xs, p x] poate fi exprimat� utiliz�nd func�iile de ordin �nalt map �i filter. -}

functio :: [a] -> (a -> Bool) -> (a -> b) -> [b]
functio xs p f = map f (filter p xs)

-- functio [1, 2, 3, 4] (\x -> x `mod` 2 == 0) (\x -> x * x)