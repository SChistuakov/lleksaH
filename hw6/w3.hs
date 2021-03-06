import Data.Maybe
import Data.List(delete,sortBy)

data Tree a   = Nill
            | Node a (Tree a) (Tree a)
                        deriving (Show,Eq)

insert :: (Ord a, Num i) => a -> Tree (a,i) -> Tree (a,i)
insert x t = insertsub x t 0

insertsub :: (Ord a, Num i) => a -> Tree (a,i) -> i -> Tree (a,i)
insertsub x Nill c = Node (x,c) Nill Nill
insertsub x (Node (a,l) left right) c
    | x == a = Node (x,c) left right
    | x < a  = Node (a,l) (insertsub x left (c+1)) right
    | x > a  = Node (a,l) left (insertsub x right (c+1))

remove :: (Ord a, Num i, Ord i) => a -> Tree (a,i) -> Tree (a,i)
remove d t = fromList (delete d (map fst (sortBy (\ x y -> compare (snd x) (snd y)) (toList t))))

toList :: Tree a -> [a]
toList t = foldt (:) [] t

fromList :: (Ord a, Num i) => [a] -> Tree (a,i) 
fromList xs = foldl (flip insert) Nill xs

search _          Nill          = Nothing
search predicate (Node (x,c) left right) = if predicate x
                                                    then Just x
                                                else if leftSearch == Nothing then rightSearch else leftSearch
                                                where leftSearch = search predicate left
                                                      rightSearch = search predicate right

foldt :: ( a -> b -> b) -> b -> Tree a -> b
foldt _ z Nill = z
foldt f z (Node x left right) = foldt f (f x (foldt f z left)) right

height :: (Tree a) -> Integer
height Nill = 0
height (Node _ left right) = 1 + max (height left) (height right)

size :: Tree a -> Integer
size t = foldt (\_ y -> y + 1) 0 t