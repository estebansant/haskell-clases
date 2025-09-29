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
eliminarUltimoDigito :: Integer -> Integer
eliminarUltimoDigito n = div n 10

leerUltimoDigito :: Integer -> Integer
leerUltimoDigito n = mod n 10

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n==0=True
                      | n < 10 = True
                      | otherwise = leerUltimoDigito n == leerUltimoDigito (eliminarUltimoDigito n) && todosDigitosIguales (eliminarUltimoDigito n)
                    
-- Ejercicio 7
cantDigitos :: Integer-> Integer
cantDigitos j | j<10 =1
              | otherwise = 1+ cantDigitos (eliminarUltimoDigito j)

iesimoDigito :: Integer->Integer->Integer
iesimoDigito n i |  cantDigitos n == i = leerUltimoDigito n
                 | otherwise = iesimoDigito (eliminarUltimoDigito n) i

-- Teorica
sumaDivisoresHasta :: Integer->Integer->Integer
sumaDivisoresHasta n k | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

productoDigitos :: Integer -> Integer
productoDigitos n | n<10 = n
                  | otherwise = leerUltimoDigito n * productoDigitos (eliminarUltimoDigito n)

sumaConExponentes :: Integer -> Integer -> Integer
sumaConExponentes n m | m==0=0
                       | otherwise = sumaConExponentes n (m-1) + n^m

sumatoriaDoble :: Integer -> Integer-> Integer
sumatoriaDoble n m | n == 0 = 0
                   | otherwise = sumatoriaDoble (n-1) m + sumaConExponentes n m

-- Ejercicio 8
sumaDigitos :: Integer->Integer
sumaDigitos n | n==0=0
              | otherwise = sumaDigitos (eliminarUltimoDigito n) + leerUltimoDigito n

-- Ejercicio 9

leerPrimerDigito :: Integer -> Integer
leerPrimerDigito n | n < 10 = n
                   | otherwise = div n (cantDigitos (n-1))

eliminarPrimerDigito :: Integer -> Integer
eliminarPrimerDigito n | n < 10 = n
                       | otherwise = n - leerPrimerDigito n * (10^cantDigitos (n-1))

esCapicua :: Integer -> Bool
esCapicua n | n < 10 = True
-- No se como seguir esta :(

--Ejercicio 10
--a)

f1 :: Integer -> Integer
f1 n | n==0=1
     | otherwise = 2^n + f1(n-1)

-- b)
f2 :: Integer -> Integer -> Integer
f2 n q | n==1=q
       | otherwise =  q^n + f2 (n-1) q

-- c)
f3 :: Integer-> Integer-> Integer
f3 n q | n == 0 = 1
       | otherwise = q^doble + f2 (doble-1) q 
       where doble = 2*n

-- d)

f4Limite :: Integer -> Integer -> Integer
f4Limite n q | n==div doble 2=q^n
             | otherwise =  q^doble  + f4Limite (n-1) q
             where doble = 2*n

f4 :: Integer -> Integer -> Integer
f4 n q | n == div n 2=q^n
       | otherwise = q^doble + f4Limite (doble-1) q 
       where doble = 2*n

-- Ejercicio 11
factorial :: Integer -> Integer
factorial n | n==0=1
            | n==1=1
            | otherwise = n*factorial(n-1)  

eAprox :: Integer -> Float
eAprox n | n==0=1
         | otherwise = 1 / fromIntegral (factorial n) + eAprox(n-1)
