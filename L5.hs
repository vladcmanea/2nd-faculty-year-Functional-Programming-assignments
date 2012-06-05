{- ªtiind cã orice numãr natural se poate exprima cu ajutorul lui Zero ºi Succesor, sã se defineascã în Haskell tipul recursiv asociat numerelor naturale. Utilizând recursia, definiþi funcþiile de adunare ºi înmulþire a numerelor naturale (add :: Nat -> Nat -> Nat, mul :: Nat -> Nat -> Nat). -}

data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add Zero (Succ n) = Succ n
add (Succ n) Zero = Succ n
add (Succ n) (Succ m) = add n (Succ (Succ m))

mul :: Nat -> Nat -> Nat
mul _ Zero = Zero
mul Zero _ = Zero
mul (Succ n) (Succ m) = Succ (add n (mul (Succ n) m))

{- Consideraþi urmãtorul tip Haskell asociat arborilor binari: data Tree = Leaf Int | Node Tree Tree. Spunem cã un arbore binar este balanced dacã pentru fiecare nod, numãrul de noduri din subarborele drept diferã cu cel mult 1 faþã de numãrul de noduri din subarborele stâng. Definiþi funcþia balanced :: Tree -> Bool, care determinã dacã un arbore binar este echilibrat sau nu. Hint: definiþi mai întâi o funcþie care returneazã numãrul de frunze dintr-un arbore. -}

data Tree = Leaf Int | Node Tree Tree deriving (Show)

count :: Tree -> Int
count (Leaf x) = 1
count (Node xt yt) = 1 + count xt + count yt

balanced :: Tree -> Bool
balanced (Leaf x) = True
balanced (Node xt yt) = balanced xt && balanced yt && -1 <= (count xt) - (count yt) && (count xt) - (count yt) <= 1

{- Definiþi o funcþie balance :: [Int] -> Tree care converteºte o listã nevidã de întregi într-un arobre binar balanced (vezi ex. 2 pentru definiþii). Hint: definiþi o funcþie auxiliarã care împarte o listã în douã liste a cãror lungimi diferã cu cel mult o unitate. -}


balance:: [Int] -> Tree
balance xs 	| length xs == 1 = Leaf (head xs)
		| length xs `mod` 2 == 0 = error "Could not make tree"
		| otherwise = Node (balance (take ((length xs) `div` 2) xs)) (balance (drop (1 + (length xs) `div` 2) xs))
