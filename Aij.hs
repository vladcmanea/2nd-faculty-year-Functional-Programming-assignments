type Matrix a = [[a]]
type Row a = [a]
type Col a = [a]

getRows :: (Num a) => Matrix a -> Int
getRows ms = case length ms of
			0 -> error "Null matrix"
			_ -> length ms

getCols :: (Num a) => Matrix a -> Int
getCols ms = case length ms of
		0 -> error "Null matrix"
		1 -> 1
		_ -> if (and (map (\(x, y) -> length x == length y) (zip ms (tail ms))))
			then length (head ms)
			else error "Could not find cols"

getAij :: (Num a) => Matrix a -> Int -> Int -> a
getAij ms i j = (ms!!(i - 1))!!(j - 1)

setAij :: (Num a) => Matrix a -> Int -> Int -> a -> Matrix a
setAij ms i j x = (take (i - 1) ms) ++ [take (j-1) (ms!!(i-1)) ++ [x] ++ drop j (ms!!(i-1))] ++ (drop i ms)