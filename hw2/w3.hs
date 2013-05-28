f :: Integer -> Integer
f 0 = 0
f n = ((mod n 10) + (f(div n 10)))