import Control.Monad.Cont


cFilter :: (a -> Bool) -> [a] ->  Cont [a] [a]
cFilter _ [] = return []
cFilter f (x:xs) = cFilter f xs >>= return . (filter f [x]!!0 :)
hfilter f xs = runCont (cFilter f xs) id

cMap :: (a -> b) -> [a] ->  Cont [b] [b]
cMap _ [] = return []
cMap f (x:xs) = cMap f xs >>= return . (f x :)
hmap f xs = runCont (cMap f xs) id