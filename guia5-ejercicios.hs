import GHC.Base (BCO)
lista1 = [1,3..100]
lista2 = [100,99..1]
lista3 = [1,0..(-100)]
lista4 = [-19,-15..20]

longitud :: [Int] -> Int
longitud [] = 0
longitud (_:xs)=1+longitud xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece a (x:xs) | a==x=True
                   | otherwise = pertenece a xs 