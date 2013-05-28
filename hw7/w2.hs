import System.Random

data Tree a = Nill | Node a (Tree a) (Tree a) deriving (Show, Eq)

tinsert :: (Ord a) => a -> Tree a -> Tree a
tinsert x Nill = Node x Nill Nill
tinsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (tinsert x left) right
    | x > a  = Node a left (tinsert x right)

foldt :: ( a -> b -> b) -> b -> Tree a -> b
foldt _ z Nill = z
foldt f z (Node x left right) = foldt f (f x (foldt f z left)) right

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Nill = Nill
tmap f (Node x left right) = Node (f x) (tmap f left) (tmap f right)

treeRand :: Tree Int -> Tree Int
treeRand =  tmap (fst .random . etStdRandom (randomR (1,100)))
