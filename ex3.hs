soma :: Int -> Int -> Int
soma x y = x+y

muxPorSoma :: Int -> Int -> Int
muxPorSoma x y
 | x==0 || y==0 = 0
 | x==1 = soma y 0
 | y==1 = soma x 0
 | otherwise = soma x (muxPorSoma x (y-1))
