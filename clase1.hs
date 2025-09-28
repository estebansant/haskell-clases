f :: Integer -> Integer
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16
    | otherwise = undefined
    
g:: Integer -> Integer
g a | a == 8 = 16
    | a == 16 = 4
    | a == 131 = 1
    | otherwise = undefined

h :: Integer -> Integer
h = f.g

k :: Integer -> Integer
k = g.f

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | ((x >= y) && (x >= z)) = x
              | ((y >= x) && (y >= z)) = y
              | ((z >= x) && (z >= y)) = z
              | otherwise = undefined

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | (x==y) && (x==z) =0
                    | (x==y) && (x/=z) =z
                    | (y==z) && (y/=x) =x
                    | (x==z) && (x/=y) =y
                    | (x/=y) && (x/=z) = x+y+z
                    | otherwise = undefined

sumaDistintosRaro :: Integer -> Integer -> Integer -> Integer
sumaDistintosRaro x y z | (x==y) && (x==z) = x
                    | (x==y) && (x/=z) =z+x
                    | (y==z) && (y/=x) =x+y
                    | (x==z) && (x/=y) =y+z
                    | (x/=y) && (x/=z) = x+y+z
                    | otherwise = undefined

digitoUnidades:: Integer -> Integer
digitoUnidades x = mod x 10

digitoDecenas:: Integer -> Integer
digitoDecenas x = mod (div x 10 ) 10
                  