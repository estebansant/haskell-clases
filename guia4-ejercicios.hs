-- Ejercicio 1
fibonacci :: Integer -> Integer
fibonacci n | n==0=0
            | n==1=1
            | otherwise = fibonacci (n-1) + fibonacci (n-2)

-- Ejercicio 2
parteEntera :: Float ->Integer
parteEntera x | x < 1 = 0
              | otherwise = parteEntera(x-1)+1

-- Ejercicio 3
esDivisible :: Integer ->Integer ->Bool
esDivisible x y | x==0  = True
                | x < y = False
                | otherwise = esDivisible (x-y) y

-- Ejercicio 4
sumaAnterioresN :: Integer -> Integer
sumaAnterioresN x | x==0=0
                  | otherwise = sumaAnterioresN (x-1)+x

contadorHastaN :: Integer -> Integer
contadorHastaN x | x==0=0
                 | otherwise = contadorHastaN (x-1)+1
        
sumaImpares :: Integer ->Integer
sumaImpares n | n==0=0
              | n==1=1
              | otherwise = sumaImpares (n-1) + (2*n-1)

-- Ejercicio 5
medioFact :: Integer-> Integer
medioFact n | n==0=1
            | n==1=1
            | otherwise = n*medioFact(n-2)

--Ejercicio 6
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n==0=True
                      | n < 10 = True
                      | otherwise = mod n 10 == mod (div n 10) 10 && todosDigitosIguales (div n 10)