msort :: (Ord a) => [a] -> [a]
msort xs 	| length xs == 0 = []
		| length xs == 1 = [head xs]
		| otherwise = merge (msort (take (length xs `div` 2) xs)) (msort (drop (length xs `div` 2) xs))

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) 	| x < y = [x] ++ merge xs (y:ys)
			| otherwise = [y] ++ merge (x:xs) ys
