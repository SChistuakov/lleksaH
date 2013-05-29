ptrim :: [Int] -> [Int]
ptrim p = if null p 
                      then [] 
                      else if (last p) /= 0 then p 
                                            else ptrim (init p)
											
pdeg :: [Int] -> Int
pdeg p = length (ptrim p) - 1

term :: [Int] -> Int -> Int
term p i = if i < (length p) then p!!i else 0
   
add :: [Int] -> [Int] -> [Int]
add p1 p2 = ptrim [(term p1 i) + (term p2 i) | i <- [0..(max (length p1) (length p2)) - 1]]
   
sub :: [Int] -> [Int] -> [Int]
sub p1 p2 = ptrim [(term p1 i) - (term p2 i) | i <- [0..(max (length p1) (length p2)) - 1]]


dev1 :: [Int] -> [Int]
dev1 p = ptrim [(i * (p!!i)) | i <- [1..(length (ptrim p)) - 1]]

dev :: [Int] -> Int -> [Int]
dev p 0 = p
dev p n = dev1 (dev p (n - 1))

tshow :: Int -> Int -> String
tshow 0 i = ""
tshow a 0 = (if (a > 0) then "+" else "-") ++ show (abs a)
tshow a 1 = (if (a == 1) then "" else if (a == -1) then "" else tshow a 0) ++ "x"
tshow a i = (if (a == 1) then "" else if (a == -1) then "" else tshow a 0) ++ "x^" ++ show i

ltshow :: [Int] -> String
ltshow p = if null p then "" else tshow (last p) ((length p) - 1)

pshow :: [Int] -> String
pshow p = ltshow (ptrim p) ++ if null p then "" else pshow (init (if null (ptrim p) then [0] else (ptrim p)))