{- afiºeazã sub formã de ºir de caractere toate numerele care se divid cu 3, dintr-o listã -}

divide31 :: (Integral a) => [a] -> [a]
divide31 xs = if (length xs) == 0 then [] else if (head xs) `mod` 3 == 0 then [xs!!0] ++ divide31 xs else divide31 xs

divide32 :: (Integral a) => [a] -> [a]
divide32 xs = case (length xs) of
			0 -> []
			_ -> case (head xs) `mod` 3 of
				0 -> [head xs] ++ divide32 (tail xs)
				_ -> divide32 (tail xs)

divide33 :: (Integral a) => [a] -> [a]
divide33 xs 	| (length xs) == 0 = []
		| (length xs) > 0 && (head xs) `mod` 3 == 0 = [head xs] ++ divide33 (tail xs)
		| otherwise = divide33 (tail xs) 

divide34 :: (Integral a) => [a] -> [a]
divide34 [] = []
divide34 (x:xs) = (if x `mod` 3 == 0 then [x] else []) ++ divide34 xs

head1 :: (Integral a) => [a] -> a
head1 xs = if (length xs) == 0 then error "Could not find head for empty list" else xs!!0

head2 :: (Integral a) => [a] -> a
head2 xs = case (length xs) of
		0 -> error "Could not find head for empty list"
		_ -> xs!!0

head3 :: (Integral a) => [a] -> a
head3 xs 	| length xs == 0 = error "Could not find head for empty list"
		| otherwise = xs!!0

head4 :: (Integral a) => [a] -> a
head4 [] = error "Could not find head for empty list"
head4 (x:xs) = x

{- Definiþi o funcþie Haskell care testeazã dacã un numãr întreg este palindrom -}

pali :: (Integral a) => a -> Bool
pali a = if a < 0 then pali (-a) else palist a == reverse (palist a)

palist :: (Integral a) => a -> [a]
palist a = if (a <= 0) then [] else palist (a `div` 10) ++ [a `mod` 10]

{- Definiþi o funcþie Haskell care insereazã un element la mijlocul unei liste -}

insert :: a -> [a] -> [a]
insert x xs = take ((length xs) `div` 2) xs ++ [x] ++ drop (((length xs) `div` 2)) xs