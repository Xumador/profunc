fat::Int->Int
fat 0 = 1
fat 1 = 1     -- Criação da base
fat x = x * (x-1)


col :: Int -> Int -> Int
col n p
 | n < p = 0
 |otherwise = div(fat n) ((fat p) * (fat(n-p)))
