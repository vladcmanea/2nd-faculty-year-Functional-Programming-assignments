comb :: Int -> Int -> Integer
comb x y 	| x < 0 = comb (-x) y
		| y < 0 = comb x (-y)
		| x < y = comb y x
		| y == 0 = 1
		| otherwise = comb (x - 1) y + comb (x - 1) (y - 1)