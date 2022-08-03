tam :: Int -> Int
tam 0 = 0
tam x = 1 + tam (div x 10)

inv :: Int -> Int
inv 0 = 0
inv x = (mod x 10)*(10^((tam x)-1)) + inv (div x 10)
