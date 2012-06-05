data (Eq a) => Stack a = Empty | Cons a (Stack a) deriving Show

isEmpty :: (Eq a) => Stack a -> Bool 
isEmpty Empty = True
isEmpty (Cons a st) = False

push :: (Eq a) => Stack a -> a -> Stack a
push Empty a = Cons a Empty
push (Cons x st) y = Cons y (Cons x st)

pop :: (Eq a) => Stack a -> Stack a
pop Empty = error "Stack is empty"
pop (Cons x st) = st

top :: (Eq a) => Stack a -> a
top Empty = error "Stack is empty"
top (Cons x st) = x