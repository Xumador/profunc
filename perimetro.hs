-- perimetro circulo
perimetro :: Float -> Float
perimetro raio = 2*pi*raio

-- area circulo
area :: Float -> Float
area raio = (raio^2)*pi

--diferenca de area com chamada recursiva
diferenca :: Float -> Float -> Float
diferenca r1 r2 = (area r1) - (area r2)

--testa maior
maior :: Int -> Int -> Int
maior a b = if a >= b
	then a
	else b
	
--testa maior usando guardas
maiorg :: Int -> Int -> Int
maiorg a b
	| a > b = a
	| a < b = b
	| otherwise = 0

--multiplica a propria func
fatorial :: Int -> Int
fatorial a
	| a == 0 = 1
	| a > 0 = a * fatorial(a-1)
	
--testa se é par
ehpar :: Int -> Bool
ehpar x = if mod x 2 == 0
	then True
	else False
	
