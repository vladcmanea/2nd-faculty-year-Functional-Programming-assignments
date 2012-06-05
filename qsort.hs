qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lt ++ [x] ++ qsort rt where
			lt = [y | y <- xs, y < x]
			rt = [y | y <- xs, y >= x]
