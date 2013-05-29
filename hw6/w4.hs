import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)
               
dijkstra :: (Ord a) => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph = 
    f (fromList [(v, (if v == source then 0 else 1/0, Nothing)) | v <- keys graph]) (keys graph) 
      where f ds [] = ds
            f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
                m = K.minimum (fst . (ds !)) q
                relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e
              
f :: (Ord a) => a -> a -> Map a [(a, Float)] -> [a]
f from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)
	
test :: (Ord a) => [(a, a, Float)] -> Map a [(a, Float)]
test g = fromListWith (++) $ g >>= \(a,b,c) -> [(a,[(b,c)]), (b,[(a,c)])]

--[('a','c',2), ('a','d',6), ('b','a',3), ('b','d',8), ('c','d',7), ('c','e',5), ('d','e',10)]