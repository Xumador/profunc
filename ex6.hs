raizSeis :: Int -> Float
raizSeis 0 = sqrt 6
raizSeis x = sqrt (6 + raizSeis (x-1))
