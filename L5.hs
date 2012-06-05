{- �tiind c� orice num�r natural se poate exprima cu ajutorul lui Zero �i Succesor, s� se defineasc� �n Haskell tipul recursiv asociat numerelor naturale. Utiliz�nd recursia, defini�i func�iile de adunare �i �nmul�ire a numerelor naturale (add :: Nat -> Nat -> Nat, mul :: Nat -> Nat -> Nat). -}

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

{- Considera�i urm�torul tip Haskell asociat arborilor binari: data Tree = Leaf Int | Node Tree Tree. Spunem c� un arbore binar este balanced dac� pentru fiecare nod, num�rul de noduri din subarborele drept difer� cu cel mult 1 fa�� de num�rul de noduri din subarborele st�ng. Defini�i func�ia balanced :: Tree -> Bool, care determin� dac� un arbore binar este echilibrat sau nu. Hint: defini�i mai �nt�i o func�ie care returneaz� num�rul de frunze dintr-un arbore. -}

data Tree = Leaf Int | Node Tree Tree deriving (Show)

count :: Tree -> Int
count (Leaf x) = 1
count (Node xt yt) = 1 + count xt + count yt

balanced :: Tree -> Bool
balanced (Leaf x) = True
balanced (Node xt yt) = balanced xt && balanced yt && -1 <= (count xt) - (count yt) && (count xt) - (count yt) <= 1

{- Defini�i o func�ie balance :: [Int] -> Tree care converte�te o list� nevid� de �ntregi �ntr-un arobre binar balanced (vezi ex. 2 pentru defini�ii). Hint: defini�i o func�ie auxiliar� care �mparte o list� �n dou� liste a c�ror lungimi difer� cu cel mult o unitate. -}


balance:: [Int] -> Tree
balance xs 	| length xs == 1 = Leaf (head xs)
		| length xs `mod` 2 == 0 = error "Could not make tree"
		| otherwise = Node (balance (take ((length xs) `div` 2) xs)) (balance (drop (1 + (length xs) `div` 2) xs))
