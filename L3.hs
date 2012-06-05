{- Utilizând conceptul de list comprehension, construiþi o expresie care calculeazã suma 1*1 + ... + 100 * 100 -}

sumsquares :: (Integral a) => a -> a
sumsquares n 	| n < 0 = error "N too small"
		| otherwise = sum [x * x | x <- [1..n]]

{- O 3-uplã (x, y, z) se numeºte pythagorean dacã x * x + y * y = z * z. Utilizând list comprehension, definiþi funcþia pyths :: Int -> [(Int, Int, Int)] care returneazã lista tuturor 3-uplelor de formã pythagorean, de o anumitã limitã. -}

pyths :: (Integral a) => a -> [(a, a, a)]
pyths x = [(a, b, c) | c <- [1..x], a <- [1..c], b <- [1..c], a * a + b * b == c * c]

{- Un numãr întreg se numeºte perfect dacã este egal cu suma divizorilor sãi, excluzând numãrul însuºi. Utilizând list comprehension ºi funcþia factors, definiþi funcþia perfects :: Int -> [Int] care returneazã lista tuturor numerelor perfecte, pânã la o anumitã limitã. -}

perfects :: Int -> [Int]
perfects x = [y | y <- [1..x], y == sum (factors y)]

factors :: Int -> [Int]
factors x = [z | z <- [1..(x `div` 2)], x `mod` z == 0]

{- Arãtaþi cum lista (datã cu list comprehension) [f x | x <- xs, p x] poate fi exprimatã utilizând funcþiile de ordin înalt map ºi filter. -}

functio :: [a] -> (a -> Bool) -> (a -> b) -> [b]
functio xs p f = map f (filter p xs)

-- functio [1, 2, 3, 4] (\x -> x `mod` 2 == 0) (\x -> x * x)