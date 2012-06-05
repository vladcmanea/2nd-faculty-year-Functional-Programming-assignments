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

add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add ms ns = if getRows ms > 0 && getRows ms == getRows ns && getCols ms == getCols ns && getCols ns > 0
		then map (\(xs, ys) -> map (\(x, y) -> x + y) (zip xs ys)) (zip ms ns)
		else error "Could not add matrix of different sizes" 

mul :: (Num a) => Matrix a -> Matrix a -> Matrix a
mul ms ns = if getRows ms > 0 && getCols ms == getRows ns && getCols ns > 0
		then matrxi 1 ms ns
		else error "Could not multiply matrix of different sizes"

matrxi :: (Num a) => Int -> Matrix a -> Matrix a -> Matrix a
matrxi i ms ns 	| i > getRows ms = []
		| otherwise = [matrxij i 1 ms ns] ++ matrxi (i + 1) ms ns

matrxij :: (Num a) => Int -> Int -> Matrix a -> Matrix a -> Row a
matrxij i j ms ns 	| j > getCols ns = []
			| otherwise = [matrxijk i j 1 ms ns] ++ matrxij i (j + 1) ms ns

matrxijk :: (Num a) => Int -> Int -> Int -> Matrix a -> Matrix a -> a
matrxijk i j k ms ns 	| k > getCols ms = 0
			| otherwise = (ms!!(i - 1))!!(k - 1) * (ns!!(k - 1))!!(j - 1) + matrxijk i j (k + 1) ms ns
