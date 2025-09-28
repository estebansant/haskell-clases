-- Ejercicio 1

f :: Integer -> Integer
f x | x==1 = 8
    | x==4 = 131
    | x==16=16
    | otherwise = 0

g :: Integer -> Integer
g y | y==8=16
    |y==16=4
    |y==131=1
    |otherwise = 0

h :: Integer -> Integer
h n = f(g n)

k :: Integer -> Integer
k n = g(f n)