import Data.Char

-- Ex 1 --------
-- a)
f1::Int->Int
f1 x
	|x>=0 = ((x+4) `div` (x+2))
	|otherwise = div 2 x

-- b)
f2::Int->Int->Int
f2 x y
	|x>=y = x+y
	|otherwise = x-y
	
-- c)
f3::Int->Int->Int->Int
f3 x y z
	|(x+y) > z = x+y+z
	|(x+y) < z = x-y-z
	|otherwise = 0 

-- Ex 2 --------
fat::Int->Int
fat 0 = 1 --linha corrigida
fat x = x * fat(x-1)
-- a função não tinha ponto de parada para a recursão

-- Ex 3 --------
soma::Int->Int->Int
soma x y = x+y

produto::Int->Int->Int
produto x y
	|x==0 || y==0 = 0
	|x==1 = soma y 0
	|y==1 = soma x 0
	|x>1 = (soma 0 y) + (produto (x-1) y)
	|y>1 = (soma x 0) + (produto x (y-1))
	
-- Ex 4 --------
invertInt::Int->Int
invertInt x = (read (invertNum (show x)))

invertNum::[Char]->[Char]
invertNum (a:[]) = [a]
invertNum (a:b) = (invertNum b) ++ [a]

-- Ex 5 --------
square :: Int->Int
square num = num*num

fourPower::Int->Int
fourPower x = square x * square x

-- Ex 6 --------
raiz6::Int->Float
raiz6 0 = sqrt (6)
raiz6 x = sqrt (6 +(raiz6 (x-1)))

-- Ex 7 --------
--https://www.todamateria.com.br/analise-combinatoria/#:~:text=sentarem%20neste%20banco.-,Combina%C3%A7%C3%B5es,-As%20combina%C3%A7%C3%B5es%20s%C3%A3o
combinacoes::Int->Int->Int
combinacoes m n = (fat m) `div` ((fat n)*(fat (m-n))) --m>=n

-- Ex 8 --------
mdc::Int->Int->Int
mdc m n 
	|mod m n == 0 = n
	|otherwise = mdc n (mod aux n)
	where
		aux = m
		
-- Ex 9 --------
howManyMultiples::Int->Int->Int->Int
howManyMultiples n a b
	|b<a = 0
	|(mod b n) == 0 = 1 + (howManyMultiples n a (b-1))
	|otherwise = howManyMultiples n a (b-1)

-- Ex 10 -------
lastDigit::Int->Int
lastDigit x = (read (lastDigit1 (show x)))

lastDigit1::[Char]->[Char]
lastDigit1 [a] = [a]
lastDigit1 (a:b) = lastDigit1 b

-- Ex 11 -------
anyDigit::Int->Int->Int
anyDigit x y = (read (anyDigit1 (x) (show y)))

anyDigit1::Int->[Char]->[Char]
anyDigit1 0 (a:b) = [a]
anyDigit1 x (a:b) = anyDigit1 (x-1) b
anyDigit1 x y
	|(x > length y) || (x < 0) = "-1"

-- Ex 12 -------
--allDifferent::Int->Int->Int->Bool
--allDifferent m n p = (m/=n) && (n/=p)
 -- a) não compara o valor de m com para
allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)

-- Ex 13 -------
howManyEqual::Int->Int->Int->Int
howManyEqual a b c 
	| (a==b) && (a==c) = 3
	| (a/=b) && (b/=c) && (a/=c) = 0
	| otherwise = 2

-- Ex 14 -------
periodo::Int
periodo = 7

-- tabela de vendas
vendas :: Int -> Int
vendas 1 = 41
vendas 2 = 72
vendas 3 = 41
vendas 4 = 2
vendas 5 = 91
vendas 6 = 55
vendas 7 = 41
vendas _ = 0

-- a){-parameters: value; interval beginning; interval ending; return value-}
howManyLess::Int->Int->Int->Int
howManyLess _ _ 0 = 0
howManyLess v i f
	|(vendas f) < v && f>=i = 1 + howManyLess v i (f-1)
	|otherwise = howManyLess v i (f-1)

-- b)
noZeroInPeriod::Int->Bool
noZeroInPeriod p
	|(howManyLess 1 1 p) == 0 = True
	|otherwise = False

-- c)
zerosInPeriod::Int->[Int]
zerosInPeriod periodo
	|periodo>0 && (vendas periodo == 0) = periodo : zerosInPeriod (periodo - 1)
	|otherwise = []
	
-- d)
listVendasBaixo::Int->Int->[Int]
listVendasBaixo _ 0 = []
listVendasBaixo v p
	|(vendas p < v) = p : (listVendasBaixo v (p-1))
	|otherwise = listVendasBaixo v (p-1)
	
-- 15 ---------
fib :: Int -> Int
fib n
	| n == 0 = 0
	| n == 1 = 1
	| n > 1 = fib (n-2) + fib (n-1)
-- ??? Falta terminar

-- 16 ---------
funny x y z
	| x> z = True
	|y >= x  = False
	|otherwise = True

funny3 x y z = not(y>=x)

-- 17 ---------
uppercase :: Char->Char
uppercase a
	|(isLower a) = (toUpper a)
	|otherwise = a

-- 18 ----------
charToNum::Char->Int
charToNum a
	|isDigit a = 64-(ord a)
	|otherwise = -1

-- 19 ----------
duplicate::String->Int->String
duplicate _ 0 = ""
duplicate a n = a ++ duplicate a (n-1)

-- 20 ----------
--TO DO
-- 21 ----------
--TO DO
-- 22 ---------
inverte :: [Int]->[Int]
inverte [a] = [a]
inverte (a:b) = (inverte b)++[a]

-- 23 ---------
par::[Int]->[Int]
par [] = []
par (a:b)
	|mod a 2 == 0 = a : par b
	|otherwise = par b

impar::[Int]->[Int]
impar [] = []
impar (a:b)
	|mod a 2 /= 0 = a : impar b
	|otherwise = impar b

separa::[Int]->([Int],[Int])
separa a = (par a, impar a)

-- 24 ----------
converte::[Int]->[Char]
converte [] = ""
converte (a:b) = chr (a+64) : converte b

-- 25 ----------
{-
(a) ['a'..'g'] = "abcdefg"
(b) [0.1 ..0.9] = [0.1,1.1]
(c) [0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.9]
(d) [0.1,0.3 ..1.8] = [0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9]
(e) [0.4,0.2 ..0.8] = []
(f) [1,4..15] = [1,4,7,10,13]
-}

-- 26 ----------
contaChar::[Char]->String->Int
contaChar [] _ = 0
contaChar (a:b) c
	|c == [a] = 1+ contaChar b c
	|otherwise = contaChar b c

-- 27 ----------
purifica::[Int]->[Int]
purifica [] = []
purifica (a:b:c)
	|a == b = a : purifica c
	|otherwise = a: b : purifica c

-- 28 ----------
repeteN::Int->Int->[Int]
repeteN _ 0 = []
repeteN n i
	|i>0 = n : repeteN n (i-1)


proliferaInt::[Int]->[Int]
proliferaInt [] = []
proliferaInt (a:b) = (repeteN a a) ++ proliferaInt b

-- 29 ----------
repeteC::Char->Int->[Char]
repeteC a 1 = [a]
repeteC a i
	|i>0 = a : repeteC a (i-1)

proliferaChar::[Char]->[Char]
proliferaChar [] = []
proliferaChar (a:b) = (repeteC a (64-(ord a))) ++ proliferaChar b

