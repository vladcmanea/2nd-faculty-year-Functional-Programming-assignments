data Figura = Triunghi Float Float Float | Patrat Float Float | Cerc Float | Poligon [(Float, Float)]

aria :: Figura -> Float

aria (Triunghi a b c) = sqrt (p * (p - a) * (p - b) * (p - c)) where p = (a + b + c) / 2
aria (Patrat l t) = t * l
aria (Cerc r) = r * r * 3.1415926535
aria (Poligon xs) =  sum (map (\ ((x2, y2), (x3, y3)) -> areaTri x2 y2 x3 y3 (head xs)) (zip (tail xs) (tail (tail xs)))) 

areaTri :: Float -> Float -> Float -> Float -> (Float, Float) -> Float
areaTri x2 y2 x3 y3 (x1, y1) = aria (Triunghi t12 t13 t23)
				where	
					t12 = sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
					t13 = sqrt((x1 - x3) * (x1 - x3) + (y1 - y3) * (y1 - y3))
					t23 = sqrt((x2 - x3) * (x2 - x3) + (y2 - y3) * (y2 - y3))

-- aria (Poligon [(1, 1), (4, 2), (5, 4), (3, 5), (2, 4)])
